use super::ast::Ast;
use super::code::{Bytecode, GFn, Stay};
use super::collections::{Arr, DequeAccess, DequeOps};
use super::encoder;
use super::engine::{glsp, stock_syms::*, with_vm, Guard, RFn, Sym};
use super::error::GResult;
use super::gc::{Root, Slot};
use super::transform;
use super::val::Val;
use super::vm::Frame;
use super::wrap::CallableOps;
use smallvec::SmallVec;
use std::cell::RefCell;
use std::collections::{
    hash_map::Entry::{Occupied, Vacant},
    HashMap,
};
use std::iter::FromIterator;
use std::rc::Rc;
use std::usize;

#[cfg(feature = "compiler")]
use super::compile::Action;

//-------------------------------------------------------------------------------------------------
// entrypoints
//-------------------------------------------------------------------------------------------------

//successively expand and evaluate a number of toplevel forms, all in the same toplevel scope.
//the `to_record` flag determines whether or not any executed Bytecodes are recorded: this is
//true for (load) calls, but false for (eval) calls.
pub(crate) fn eval(forms: &[Val], env_mode: Option<EnvMode>, to_record: bool) -> GResult<Val> {
    let mut context = Context::new(env_mode);

    //once fully-expanded, these forms all require special handling: (splice), (let-macro), (let),
    //(defer), (defer-yield). to facilitate (splice), we need an input stack.
    let mut input = Vec::from_iter(forms.iter().rev().cloned());
    let mut result = Ok(Val::Nil);

    while let Some(form) = input.pop() {
        let expanded = match fully_expand_form(&form, &mut context) {
            Ok(Some(expanded)) => expanded,
            Ok(None) => form,
            Err(error) => {
                let mut defer_result: GResult<Val> = Err(error);
                context.pop_defers(&mut defer_result);
                return defer_result;
            }
        };

        if expanded.is_arr() && expanded.clone().unwrap_arr().len() > 0 {
            let arr = expanded.clone().unwrap_arr();
            match arr.get(0)? {
                Val::Sym(SPLICE_SYM) => {
                    input.extend(arr.iter().skip(1).rev());
                }
                Val::Sym(LET_MACRO_SYM) => {
                    result = context.push_let_macro_binding(arr).map(|_| Val::Nil);
                }
                Val::Sym(DEFER_SYM) => {
                    result = context.push_defer(arr).map(|_| Val::Nil);
                }
                Val::Sym(DEFER_YIELD_SYM) => {
                    result = Ok(Val::Nil);
                }
                Val::Sym(LET_SYM) => {
                    result = context.register_toplevel_let(arr).map(|_| Val::Nil);
                }
                _ => result = evaluate_form(&expanded, &context.toplevel_lets, to_record),
            }
        } else {
            result = evaluate_form(&expanded, &context.toplevel_lets, to_record)
        }

        if let Err(error) = result {
            let mut defer_result: GResult<Val> = Err(error);
            context.pop_defers(&mut defer_result);
            return defer_result;
        }
    }

    context.pop_defers(&mut result);
    result
}

//successively expand a number of toplevel forms, all in the same toplevel scope.
//`collapse_splices` will always return an output Vec of length 1, wrapping multiple results
//in a (splice ...) form if necessary.
pub(crate) fn expand(
    forms: &[Val],
    env_mode: Option<EnvMode>,
    collapse_splices: bool,
) -> GResult<Vec<Val>> {
    let mut context = Context::new(env_mode);

    //we need to detect and handle toplevel (splice) and toplevel (let-macro). to facilitate
    //(splice), we need separate input and output stacks.
    let mut input = Vec::from_iter(forms.iter().rev().cloned());
    let mut output = Vec::<Val>::new();

    while let Some(form) = input.pop() {
        let expanded = fully_expand_form(&form, &mut context)?.unwrap_or(form);

        if expanded.is_arr() && expanded.clone().unwrap_arr().len() > 0 {
            let arr = expanded.clone().unwrap_arr();
            match arr.get(0)? {
                Val::Sym(SPLICE_SYM) => {
                    input.extend(arr.iter().skip(1).rev());
                }
                Val::Sym(LET_MACRO_SYM) => {
                    context.push_let_macro_binding(arr)?;
                    output.push(Val::Nil);
                }
                _ => output.push(expanded),
            }
        } else {
            output.push(expanded);
        }
    }

    //we suppress splices in this roundabout way so that the children of forms like
    //(splice (splice) (macro-invocation)) will still be expanded as normal
    if collapse_splices && output.len() != 1 {
        let splice = Val::Arr(arr![SPLICE_SYM, ..output]);
        output.clear();
        output.push(splice);
    }

    Ok(output)
}

//perform a single step of the expansion algorithm, with the option to override the expander
pub(crate) fn expand_1(
    form: &Val,
    expander: Option<Expander>,
    env_mode: Option<EnvMode>,
) -> GResult<Expansion> {
    let mut context = Context::new(env_mode);
    maybe_call_expander(form, expander, &mut context)
}

/**
The return value for [`glsp::expand_1`](fn.expand_1.html).
*/

#[derive(Clone, PartialEq, Debug)]
pub enum Expansion {
    ExpandedTo(Val),
    MacroNoOp,
    NotAMacro,
}

/**
A type-erased `expander`.

Used as a parameter or return value by [`glsp::bind_macro`](fn.bind_macro.html),
[`glsp::expand_1`](fn.expand_1.html), and similar functions.
*/

#[derive(Clone, Debug)]
pub enum Expander {
    GFn(Root<GFn>),
    RFn(Root<RFn>),
}

/**
A lexical-environment capture mode.

Used as a parameter by [`glsp::eval`](fn.eval.html), [`glsp::expand`](fn.expand.html), and
similar functions.
*/

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum EnvMode {
    Fresh,
    Copied,
}

impl Default for EnvMode {
    fn default() -> EnvMode {
        EnvMode::Fresh
    }
}

//an initial expansion environment. this is made aliasable (as an Rc<Env>) so that we can store a
//copy of it in the Runtime when running a macro expander. then, when `env_mode` is Copied, the
//innermost Env can be copied into the new Context. we don't support toplevel (let)s in Env,
//because we'd also need to support local (let)s for consistency, which would be challenging.
#[derive(Clone, Default)]
pub(crate) struct Env {
    let_macros: RefCell<HashMap<Sym, Root<GFn>>>,
}

//-------------------------------------------------------------------------------------------------
// Context
//-------------------------------------------------------------------------------------------------

//each entrypoint function creates a Context with which to expand or evaluate forms. it contains
//the state required by (let-macro), toplevel (let) and toplevel (defer).

struct Context {
    //keys are the variable bound by the toplevel `let`, values are the Stay used for storage.
    //when the same variable name is bound twice, the old one is simply erased from this map.
    toplevel_lets: HashMap<Sym, Root<Stay>>,

    //the active (let-macro)s. a reference to the current Env is stored in the Runtime every
    //time we invoke an expander, and it can be retrieved using glsp::env() or (env).
    env: Rc<Env>,

    //this stack is required so that we can keep the Env up to date when (let-macro) bindings
    //shadow one another. the Option is the previous binding which is being shadowed, if any.
    let_stack: Vec<(Sym, Option<Root<GFn>>)>,

    //toplevel `defer`s are a bit simpler: we compile them when they're encountered, but we save
    //their bytecode rather than running it. we run that bytecode after the last toplevel form,
    //with some juggling to ensure that errors are reported properly. `defer-yield` is a silent
    //no-op (it's not even compiled) because there's no way to yield from the toplevel.
    defers: Vec<Root<Bytecode>>,
}

impl Context {
    fn new(env_mode: Option<EnvMode>) -> Context {
        let env = match env_mode {
            None | Some(EnvMode::Fresh) => Rc::new(Env::default()),
            Some(EnvMode::Copied) => {
                let env = match glsp::env() {
                    Some(env) => (*env).clone(),
                    None => Env::default(),
                };

                Rc::new(env)
            }
        };

        Context {
            toplevel_lets: HashMap::new(),
            env,
            let_stack: Vec::new(),
            defers: Vec::new(),
        }
    }

    fn push_let_macro_binding(&mut self, form: Root<Arr>) -> GResult<()> {
        //we extract the name, params and body from (let-macro name params ..body), use them to
        //make a gfn by evaluating (fn params ..body), then bind that gfn to `name` as a macro.
        let span = form.span();
        ensure_at!(
            span,
            form.len() >= 3,
            "let-macro form has too few arguments"
        );

        let name = match form.get(1)? {
            Val::Sym(name) => name,
            _ => bail_at!(span, "the first argument to `let-macro` must be a sym"),
        };

        let params_arr = match form.get(2)? {
            Val::Arr(arr) => arr,
            _ => bail_at!(span, "the second argument to `let-macro` must be an arr"),
        };

        let fn_arr = arr![FN_SYM, FLAG_NAME_SYM, name, params_arr];
        fn_arr.set_span(span);
        fn_arr.extend(form.iter().skip(3))?;

        //we have to expand the (fn ...) form here, rather than simply calling eval(), so that the
        //expansion is affected by any current `let-macro` bindings. we don't allow let-macros to
        //access toplevel `let`s, even though we could, because it would be inconsistent with
        //the fact that they can't access non-toplevel `let`s.
        let fn_form = Val::Arr(fn_arr);
        let expanded_fn = fully_expand_form(&fn_form, self)?.unwrap_or(fn_form);

        let gfn = match evaluate_form(&expanded_fn, &HashMap::new(), false)? {
            Val::GFn(gfn) => gfn,
            _ => panic!(),
        };

        //assign the binding to the env and the let_stack
        match self.env.let_macros.borrow_mut().entry(name) {
            Occupied(mut occupied) => {
                let old_entry = occupied.insert(gfn);
                self.let_stack.push((name, Some(old_entry)));
            }
            Vacant(vacant) => {
                vacant.insert(gfn);
                self.let_stack.push((name, None));
            }
        }

        Ok(())
    }

    fn pop_binding(&mut self) {
        let (name, shadowed_entry) = self.let_stack.pop().unwrap();

        match self.env.let_macros.borrow_mut().entry(name) {
            Occupied(mut occupied) => {
                if let Some(shadowed_entry) = shadowed_entry {
                    occupied.insert(shadowed_entry);
                } else {
                    occupied.remove();
                }
            }
            Vacant(_) => panic!(),
        }
    }

    fn lookup_let_macro(&mut self, name: Sym) -> Option<Root<GFn>> {
        self.env.let_macros.borrow().get(&name).cloned()
    }

    fn register_toplevel_let(&mut self, form: Root<Arr>) -> GResult<()> {
        let span = form.span();
        if form.len() != 3 {
            bail_at!(span, "the (let ...) special form expects two arguments")
        }

        let name = match form.get::<Val>(1)? {
            Val::Sym(name) => name,
            _ => bail_at!(span, "(let ...) special form has invalid arguments"),
        };
        let init_form: Val = form.get(2)?;

        let stay = glsp::alloc(Stay::new(Slot::Nil));

        #[cfg(feature = "compiler")]
        glsp::record_action(Action::ToplevelLet(stay.clone()));

        let init_val = evaluate_form(&init_form, &self.toplevel_lets, true)?;
        stay.set(Slot::from_val(&init_val));

        //we don't actually bind the name until after its initializer has been evaluated,
        //for consistency with non-toplevel lets
        self.toplevel_lets.insert(name, stay.clone());

        Ok(())
    }

    fn push_defer(&mut self, form: Root<Arr>) -> GResult<()> {
        //paste the form's body into a (do) form
        let do_form = arr![DO_SYM];
        do_form.extend(form.iter().skip(1))?;

        //compile that (do) form into bytecode without evaluating it
        let mut ast = Ast::new();
        let node = ast.node_from_val(&Val::Arr(do_form), glsp::generated_span())?;
        transform::standard_passes(&mut ast, node);

        let bytecode = encoder::encode_fragment(&ast, node, &self.toplevel_lets)?;

        //stash that bytecode in the Context
        self.defers.push(bytecode);
        Ok(())
    }

    fn pop_defers(&mut self, result: &mut GResult<Val>) {
        //evaluate each toplevel (defer) form in reverse order. if one of them fails, chain its
        //error onto the GResult and then move on to the next (defer) form
        for bytecode in self.defers.drain(..).rev() {
            #[cfg(feature = "compiler")]
            glsp::record_action(Action::Execute(bytecode.clone()));

            let defer_result = with_vm(|vm| vm.exec_bytecode(&bytecode));

            if let Err(error) = defer_result {
                match *result {
                    Ok(_) => *result = Err(error),
                    Err(ref mut prev) => prev.chain_defer_error(error),
                }
            }
        }
    }
}

//-------------------------------------------------------------------------------------------------
// the implementation
//-------------------------------------------------------------------------------------------------

//evaluates a form which has already been fully expanded.
fn evaluate_form(
    form: &Val,
    toplevel_lets: &HashMap<Sym, Root<Stay>>,
    _to_record: bool,
) -> GResult<Val> {
    let mut ast = Ast::new();
    let node = ast.node_from_val(form, glsp::generated_span())?;
    transform::standard_passes(&mut ast, node);

    let bytecode = encoder::encode_fragment(&ast, node, toplevel_lets)?;

    #[cfg(feature = "compiler")]
    if _to_record {
        glsp::record_action(Action::Execute(bytecode.clone()));
    }

    with_vm(|vm| vm.exec_bytecode(&bytecode))
}

//recursively performs the full expansion algorithm for a single form. note that this fn's result
//may be a (splice) form, but any child (splice) forms are handled appropriately. similarly,
//if the expansion result is a (let-macro) then it's just returned, but inner (let-macro)s
//are handled properly.
fn fully_expand_form(form: &Val, context: &mut Context) -> GResult<Option<Val>> {
    let mut form = form.clone();
    let mut form_mutated = false;

    loop {
        //step 1: if it's not an array, or if it's an empty array, do nothing.
        if !form.is_arr() {
            return if form_mutated {
                Ok(Some(form))
            } else {
                Ok(None)
            };
        }

        let mut arr = form.unwrap_arr();
        if arr.len() == 0 {
            return if form_mutated {
                Ok(Some(Val::Arr(arr)))
            } else {
                Ok(None)
            };
        }

        //the value being expanded is copy-on-write. if nothing changes, there's no need to clone
        //the form, and we return None. this helps to minimize copying.
        let mut arr_owned = false;

        //expand_element repeatedly (splice)s and fully-expands the form at a single array index,
        //in-place, cloning the array if it needs to be mutated. bear in mind that empty (splice)s
        //will cause an element to be deleted - we continue looping until its replacement, if any,
        //is fully-expanded.
        fn expand_element(
            mut arr: Root<Arr>,
            i: usize,
            arr_owned: &mut bool,
            context: &mut Context,
        ) -> GResult<Root<Arr>> {
            loop {
                if arr.len() <= i {
                    return Ok(arr);
                }

                let elem = arr.get::<Val>(i).unwrap();

                if elem.is_arr()
                    && elem.clone().unwrap_arr().len() > 0
                    && elem.clone().unwrap_arr().get::<Val>(0).unwrap() == Val::Sym(SPLICE_SYM)
                {
                    if !*arr_owned {
                        arr = arr.shallow_clone();
                        *arr_owned = true;
                    }

                    let to_splice = elem.clone().unwrap_arr();
                    if to_splice.len() == 1 {
                        let _discarded: Val = arr.remove(i)?;
                        if arr.len() <= i {
                            return Ok(arr);
                        }
                    } else {
                        arr.set(i, to_splice.get::<Val>(1).unwrap())?;

                        //this is wildly inefficient, but (splice) is usually small so it probably
                        //doesn't matter. todo: revisit this when we have DequeOps::insert_multi()
                        for src_i in (2..to_splice.len()).rev() {
                            arr.insert(i + 1, to_splice.get::<Val>(src_i).unwrap())?;
                        }
                    }
                } else {
                    //note that there's no easy way to check two forms for deep-equality: all we
                    //can do is check whether they've been reallocated. therefore, to avoid
                    //endless loops here, we *must not* take ownership of an arr unless we're
                    //actually mutating it in some way.
                    match fully_expand_form(&elem, context)? {
                        Some(replacement) => {
                            if !*arr_owned {
                                arr = arr.shallow_clone();
                                *arr_owned = true;
                            }

                            arr.set(i, replacement)?;
                        }
                        None => return Ok(arr),
                    }
                }
            }
        }

        //step 2: recurse for the array's first element
        arr = expand_element(arr, 0, &mut arr_owned, context)?;

        //step 3: try to call an expander for this form *once*. if an expander was called and it
        //wasn't a macro-no-op, replace the form with the expansion result and start over from 1.
        match maybe_call_expander(&Val::Arr(arr.clone()), None, context)? {
            Expansion::ExpandedTo(expanded_to) => {
                form = expanded_to;

                //the `expanded_to` form completely replaces the arr, and it could be *anything*
                //returned from a macro, including something immutable or aliased. don't get any
                //clever ideas about setting `arr_owned` to true here...

                form_mutated = true;
                continue;
            }
            Expansion::MacroNoOp | Expansion::NotAMacro => (),
        }

        //step 4: the callee form is now fully-expanded. recurse for the "argument" forms.
        //some callees have a special traversal order, and they may permit (let-macro) children,
        //which we'll need to process.
        fn expand_do_elements(
            mut arr: Root<Arr>,
            first_child_i: usize,
            arr_owned: &mut bool,
            context: &mut Context,
        ) -> GResult<Root<Arr>> {
            let mut let_macro_count = 0;

            let mut i = first_child_i;
            while i < arr.len() {
                arr = expand_element(arr, i, arr_owned, context)?;

                if arr.len() <= i {
                    break;
                }

                if let Val::Arr(expanded_arr) = arr.get::<Val>(i)? {
                    if expanded_arr.len() >= 1
                        && expanded_arr.get::<Val>(0)? == Val::Sym(LET_MACRO_SYM)
                    {
                        context.push_let_macro_binding(expanded_arr)?;
                        let_macro_count += 1;

                        //we replace (let-macro) forms with #n so they won't upset the evaluator
                        if !*arr_owned {
                            arr = arr.shallow_clone();
                            *arr_owned = true;
                        }

                        arr.set(i, Val::Nil)?;
                    }
                }

                i += 1;
            }

            for _ in 0..let_macro_count {
                context.pop_binding();
            }

            Ok(arr)
        }

        /*fn expand_fn_params(
            mut fn_arr: Root<Arr>,
            fn_arr_owned: &mut bool,
            context: &mut Context
        ) -> GResult<Root<Arr>> {
            //for params, we *mostly* want to skip them, but we still need to expand the
            //initializers for optional arguments
            assert!(fn_arr.len() >= 2);
            let mut params = match fn_arr.get(1)? {
                Val::Arr(params) => params,
                _ => bail_at!(fn_arr.span(), "invalid (fn) form passed to expander")
            };

            //if there are no &opt params, we do nothing
            let rest_index = params.iter().rposition(|val| val == Val::Sym(REST_SYM))
                                          .unwrap_or(params.len());

            let opt_index = match params.iter().rposition(|val| val == Val::Sym(OPT_SYM)) {
                Some(opt_index) if opt_index < rest_index => opt_index,
                _ => return Ok(fn_arr)
            };

            //iterate over each &opt param. when it's a nonempty arr, in-place-expand each of its
            //elements beyond the first.
            let mut params_owned = false;

            for param_i in opt_index + 1 .. rest_index {
                let param: Val = params.get(param_i)?;
                match param {
                    Val::Arr(mut param) => {
                        let mut param_owned = false;
                        let mut elem_i = 1;
                        while elem_i < param.len() {
                            param = expand_element(param, elem_i, &mut param_owned, context)?;
                            elem_i += 1;
                        }

                        //replacing any param has a cascading effect for the entire (fn) form...
                        if param_owned {
                            if !params_owned {
                                params = params.shallow_clone();
                                params_owned = true;

                                if !*fn_arr_owned {
                                    fn_arr = fn_arr.shallow_clone();
                                    *fn_arr_owned = true;
                                }

                                fn_arr.set(1, &params)?;
                            }

                            params.set(param_i, param)?;
                        }
                    }
                    _ => ()
                }
            }

            Ok(fn_arr)
        }*/

        match arr.get::<Val>(0).unwrap() {
            Val::Sym(QUOTE_SYM) => {
                //the argument to a (quote) form is not expanded
                ensure_at!(
                    arr.span(),
                    arr.len() == 2,
                    "invalid (quote) form passed to expander"
                );
            }
            Val::Sym(SPLICE_SYM) | Val::Sym(LET_MACRO_SYM) => {
                //we return (splice) and (let-macro) to the caller unchanged, without attempting
                //any further expansion at this stage.
            }
            Val::Sym(DO_SYM) | Val::Sym(DEFER_SYM) => {
                //the arguments to a (do) special form may be (let-macro), so we need to handle
                //that: pushing any (let-macro) children and then popping them at the end.
                arr = expand_do_elements(arr, 1, &mut arr_owned, context)?;
            }
            Val::Sym(BLOCK_SYM) => {
                //for a (block) form, we expand its first form like a function argument and its
                //subsequent forms as an implicit (do)
                ensure_at!(
                    arr.span(),
                    arr.len() >= 2,
                    "invalid (block) form passed to expander"
                );
                arr = expand_element(arr, 1, &mut arr_owned, context)?;
                arr = expand_do_elements(arr, 2, &mut arr_owned, context)?;
            }
            Val::Sym(FN_SYM) => {
                //for a (fn) form, we skip expanding the parameter list, and we treat its body
                //as an implicit "do"
                let params_i = match arr.iter().position(|v| v.is_arr()) {
                    Some(i) => i,
                    None => bail_at!(arr.span(), "invalid (fn) form passed to expander"),
                };
                //arr = expand_fn_params(arr, &mut arr_owned, context)?;
                arr = expand_do_elements(arr, params_i + 1, &mut arr_owned, context)?;

                //we currently don't need to expand the parameter list of a (fn) special form at
                //all. see macros.rs: the output of the (fn) macro guarantees that any initializer
                //forms which remain in the param list are trivial, e.g. 5 or 'something.
            }
            _ => {
                //for all other forms, like (if) and function calls, we can just expand their
                //arguments in order. we forbid (let-macro) forms outside of an implicit "do".
                let mut i = 1;
                while i < arr.len() {
                    arr = expand_element(arr, i, &mut arr_owned, context)?;
                    i += 1;
                }
            }
        }

        //all finished! if the array was never mutated, it's always safe to return None.
        return if arr_owned || form_mutated {
            Ok(Some(Val::Arr(arr)))
        } else {
            Ok(None)
        };
    }
}

//performs step 3 of the expansion algorithm, once. this is the same functionality exposed as the
//(expand-1) function: it can optionally receive a custom expander, and it notifies the caller
//whether or not an expander was called, and whether or not (macro-no-op) occurred.
fn maybe_call_expander(
    form: &Val,
    expander: Option<Expander>,
    context: &mut Context,
) -> GResult<Expansion> {
    fn invoke_macro_expander(
        arr: &Root<Arr>,
        overridden: bool,
        expander: &Expander,
        context: &mut Context,
    ) -> GResult<Expansion> {
        let prev_expanding = glsp::enter_expander(arr, Rc::clone(&context.env));
        let _guard = Guard::new(|| glsp::leave_expander(prev_expanding));

        let args = SmallVec::<[Val; 16]>::from_iter(arr.iter().skip(1));

        let result = match expander {
            Expander::RFn(ref rfn) => {
                let override_name = if overridden { Some(rfn.name()) } else { None };

                glsp::push_frame(Frame::Expand(arr.to_raw(), override_name));
                let _guard = Guard::new(|| glsp::pop_frame());

                glsp::call(rfn, &args[..])
            }
            Expander::GFn(ref gfn) => {
                let override_name = if overridden { Some(gfn.name()) } else { None };

                glsp::push_frame(Frame::Expand(arr.to_raw(), override_name));
                let _guard = Guard::new(|| glsp::pop_frame());

                glsp::call(gfn, &args[..])
            }
        };

        match result {
            Ok(val) => Ok(Expansion::ExpandedTo(val)),
            Err(err) => {
                if err.is_macro_no_op() {
                    Ok(Expansion::MacroNoOp)
                } else {
                    Err(err)
                }
            }
        }
    }

    match expander {
        Some(expander) => {
            ensure!(
                form.is_arr(),
                "attempted to macro-expand a form with a custom expander, \
                                    but the form is not an arr"
            );
            let arr = form.clone().unwrap_arr();

            ensure!(
                arr.len() > 0,
                "attempted to macro-expand a form with a custom expander, \
                                    but the form is an empty arr"
            );
            ensure!(
                arr.get::<Val>(0)?.is_sym(),
                "attempted to macro-expand a form with a custom \
                                                  expander, but it doesn't start with a sym"
            );

            invoke_macro_expander(&arr, true, &expander, context)
        }
        None => {
            match *form {
                Val::Arr(ref arr) if arr.len() > 0 && arr.get::<Val>(0).unwrap().is_sym() => {
                    let sym = arr.get::<Sym>(0).unwrap();

                    //local macro bindings
                    if let Some(expander_gfn) = context.lookup_let_macro(sym) {
                        let expander = Expander::GFn(expander_gfn.clone());
                        return invoke_macro_expander(&arr, false, &expander, context);
                    }

                    //global macro bindings
                    if glsp::has_macro(sym).unwrap() {
                        let expander = glsp::get_macro(sym).unwrap();
                        return invoke_macro_expander(&arr, false, &expander, context);
                    }

                    Ok(Expansion::NotAMacro)
                }
                _ => Ok(Expansion::NotAMacro),
            }
        }
    }
}
