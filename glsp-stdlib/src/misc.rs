use glsp::{
    arr, bail, ensure, eprn, macro_no_op, stock_syms::*, str, Arr, Callable, CallableOps, Coro,
    CoroState, DequeOps, EnvMode, Expander, Expansion, FromVal, GFn, GResult, Num, RData, Rest,
    Root, Str, Sym, Val, GC_DEFAULT_RATIO, GC_MIN_RATIO,
};
use smallvec::SmallVec;
use std::char;
use std::convert::TryFrom;
use std::io::Write;
use std::iter::once;
use std::time::UNIX_EPOCH;
use std::{i32, str};

pub fn init(sandboxed: bool) -> GResult<()> {
    if !sandboxed {
        glsp::bind_rfn("load", &load)?;
        glsp::bind_rfn("require", &require)?;
    }

    glsp::bind_rfn("type-of", &type_of)?;
    glsp::bind_rfn("nil?", &nilp)?;
    glsp::bind_rfn("num?", &nump)?;
    glsp::bind_rfn("int?", &intp)?;
    glsp::bind_rfn("flo?", &flop)?;
    glsp::bind_rfn("char?", &charp)?;
    glsp::bind_rfn("bool?", &boolp)?;
    glsp::bind_rfn("sym?", &symp)?;
    glsp::bind_rfn("deque?", &dequep)?;
    glsp::bind_rfn("arr?", &arrp)?;
    glsp::bind_rfn("str?", &strp)?;
    glsp::bind_rfn("tab?", &tabp)?;
    glsp::bind_rfn("iter?", &iterp)?;
    glsp::bind_rfn("iterable?", &iterablep)?;
    glsp::bind_rfn("obj?", &objp)?;
    glsp::bind_rfn("class?", &classp)?;
    glsp::bind_rfn("fn?", &fnp)?;
    glsp::bind_rfn("rfn?", &rfnp)?;
    glsp::bind_rfn("coro?", &corop)?;
    glsp::bind_rfn("rdata?", &rdatap)?;
    glsp::bind_rfn("callable?", &callablep)?;
    glsp::bind_rfn("expander?", &expanderp)?;

    glsp::bind_rfn("same?", &samep)?;
    glsp::bind_rfn("eq?", &eqp)?;
    glsp::bind_rfn("keys-eqv?", &keys_eqvp)?;

    glsp::bind_rfn("==any?", &num_eq_anyp)?;
    glsp::bind_rfn("same-any?", &same_anyp)?;
    glsp::bind_rfn("eq-any?", &eq_anyp)?;

    glsp::bind_rfn("int", &int)?;
    glsp::bind_rfn("flo", &flo)?;
    glsp::bind_rfn("char", &char)?;
    glsp::bind_rfn("bool", &bool)?;
    glsp::bind_rfn("sym", &sym)?;

    glsp::bind_rfn("int->str", &int_to_str)?;
    glsp::bind_rfn("flo->str", &flo_to_str)?;
    glsp::bind_rfn("valid-sym-char?", &valid_sym_charp)?;
    glsp::bind_rfn("valid-sym-str?", &valid_sym_strp)?;
    glsp::bind_rfn("representable-sym-str?", &representable_sym_strp)?;

    glsp::bind_rfn("global", &global)?;
    glsp::bind_rfn("global=", &set_global)?;
    glsp::bind_rfn("global-opt", &global_opt)?;
    glsp::bind_rfn("global-opt=", &set_global_opt)?;
    glsp::bind_rfn("freeze-global!", &freeze_global)?;
    glsp::bind_rfn("has-global?", &has_global)?;
    glsp::bind_rfn("bind-global!", &bind_global)?;
    glsp::bind_rfn("del-global!", &del_global)?;

    glsp::bind_rfn("macro", &get_macro)?;
    glsp::bind_rfn("macro=", &set_macro)?;
    glsp::bind_rfn("macro-opt", &macro_opt)?;
    glsp::bind_rfn("macro-opt=", &set_macro_opt)?;
    glsp::bind_rfn("has-macro?", &has_macro)?;
    glsp::bind_rfn("bind-macro!", &bind_macro)?;
    glsp::bind_rfn("del-macro!", &del_macro)?;
    glsp::bind_rfn("expand", &expand)?;
    glsp::bind_rfn("expand-multi", &expand_multi)?;
    glsp::bind_rfn("expand-1", &expand_1)?;
    glsp::bind_rfn("macro-no-op", &macro_no_op)?;

    glsp::bind_rfn("fn-name", &fn_name)?;
    glsp::bind_rfn("fn-yields?", &fn_yieldsp)?;
    glsp::bind_rfn("arg-limits", &arg_limits)?;
    glsp::bind_rfn("min-args", &min_args)?;
    glsp::bind_rfn("max-args", &max_args)?;

    glsp::bind_rfn("coro-state", &coro_state)?;
    glsp::bind_rfn("coro-run", &coro_run)?;
    glsp::bind_rfn("coro-finish!", &coro_finish)?;

    glsp::bind_rfn("gc", &gc)?;
    glsp::bind_rfn("gc-value", &gc_value)?;
    glsp::bind_rfn("gc-value=", &set_gc_value)?;

    #[cfg(not(target_arch = "wasm32"))]
    glsp::bind_rfn("time", &time)?;
    glsp::bind_rfn("unix-time", &unix_time)?;
    glsp::bind_rfn("sleep", &sleep)?;

    glsp::bind_rfn("bail", &bail)?;
    glsp::bind_rfn("try-call", &try_call)?;
    glsp::bind_rfn("stack-trace", &stack_trace)?;
    glsp::bind_rfn("file-location", &file_location)?;

    glsp::bind_rfn("dump-form", &dump_form)?;
    glsp::bind_rfn("dump-fn", &dump_fn)?;
    glsp::bind_rfn("dump-macro", &dump_macro)?;

    glsp::bind_rfn("not", &not)?;
    glsp::bind_rfn("gensym", &gensym)?;
    glsp::bind_rfn("freed?", &freedp)?;
    glsp::bind_rfn("clone", &clone)?;
    glsp::bind_rfn("deep-clone", &deep_clone)?;
    glsp::bind_rfn("freeze!", &freeze)?;
    glsp::bind_rfn("deep-freeze!", &deep_freeze)?;
    glsp::bind_rfn("load-str", &load_str)?;
    glsp::bind_rfn("eval", &eval)?;
    glsp::bind_rfn("eval-multi", &eval_multi)?;
    glsp::bind_rfn("no-op", &no_op)?;
    glsp::bind_rfn("identity", &identity)?;

    Ok(())
}

fn type_of(arg: Val) -> Sym {
    match arg {
        Val::Nil => NIL_SYM,
        Val::Int(_) => INT_SYM,
        Val::Flo(_) => FLO_SYM,
        Val::Char(_) => CHAR_SYM,
        Val::Bool(_) => BOOL_SYM,
        Val::Sym(_) => SYM_SYM,
        Val::RFn(_) => RFN_SYM,
        Val::Arr(_) => ARR_SYM,
        Val::Str(_) => STR_SYM,
        Val::Tab(_) => TAB_SYM,
        Val::GIter(_) => ITER_SYM,
        Val::Obj(_) => OBJ_SYM,
        Val::Class(_) => CLASS_SYM,
        Val::GFn(_) => FN_SYM,
        Val::Coro(_) => CORO_SYM,
        Val::RData(_) => RDATA_SYM,
    }
}

macro_rules! builtin_typecheck {
    ($fn_name:ident, $met_name:ident) => {
        fn $fn_name(arg: Val) -> bool {
            arg.$met_name()
        }
    };
}

builtin_typecheck!(nilp, is_nil);
builtin_typecheck!(nump, is_num);
builtin_typecheck!(intp, is_int);
builtin_typecheck!(flop, is_flo);
builtin_typecheck!(charp, is_char);
builtin_typecheck!(boolp, is_bool);
builtin_typecheck!(symp, is_sym);
builtin_typecheck!(dequep, is_deque);
builtin_typecheck!(arrp, is_arr);
builtin_typecheck!(strp, is_str);
builtin_typecheck!(tabp, is_tab);
builtin_typecheck!(iterp, is_giter);
builtin_typecheck!(iterablep, is_iterable);
builtin_typecheck!(objp, is_obj);
builtin_typecheck!(classp, is_class);
builtin_typecheck!(callablep, is_callable);
builtin_typecheck!(expanderp, is_expander);
builtin_typecheck!(fnp, is_gfn);
builtin_typecheck!(rfnp, is_rfn);
builtin_typecheck!(corop, is_coro);
builtin_typecheck!(rdatap, is_rdata);

fn int(arg: Val) -> GResult<i32> {
    match arg {
        Val::Int(i) => Ok(i),
        Val::Flo(f) => {
            //corner cases are resolved as: nan -> 0, inf -> i32::MAX, -inf -> i32::MIN
            Ok(f as i32)
        }
        Val::Char(c) => {
            //all possible char values are also valid i32 values
            Ok(c as i32)
        }
        Val::Bool(b) => Ok(if b { 1 } else { 0 }),
        arg => bail!("could not cast {} to an int", arg.a_type_name()),
    }
}

fn flo(arg: Val) -> GResult<f32> {
    match arg {
        Val::Int(i) => Ok(i as f32),
        Val::Flo(f) => Ok(f),
        Val::Char(c) => Ok(c as i32 as f32),
        Val::Bool(b) => Ok(if b { 1.0 } else { 0.0 }),
        arg => bail!("could not cast {} to a flo", arg.a_type_name()),
    }
}

fn char(arg: Val) -> GResult<char> {
    let i = match arg {
        Val::Int(i) => i,
        Val::Flo(f) => f as i32,
        Val::Char(c) => return Ok(c),
        _ => bail!("could not cast {} to a char", arg.a_type_name()),
    };

    ensure!(i >= 0, "{} is not a valid char value", arg);

    match char::try_from(i as u32) {
        Ok(ch) => Ok(ch),
        Err(_) => bail!("{} is not a valid char value", arg),
    }
}

fn bool(arg: Val) -> bool {
    match arg {
        Val::Bool(b) => b,
        Val::Nil => false,
        _ => true,
    }
}

fn sym(rest: Rest<Val>) -> GResult<Sym> {
    //construct the sym's name string on the stack
    let mut bytes = SmallVec::<[u8; 128]>::new();

    for arg in rest.iter() {
        write!(&mut bytes, "{}", arg).unwrap();
    }

    if bytes.is_empty() {
        bail!("cannot construct a sym from an empty str")
    }

    let st = str::from_utf8(&bytes[..]).unwrap();

    //create and return the sym
    glsp::sym(st)
}

fn int_to_str(arg: i32, opt_radix: Option<usize>) -> GResult<Root<Str>> {
    let radix = opt_radix.unwrap_or(10);
    ensure!((2..=36).contains(&radix), "invalid radix {}", radix);

    let chars = [
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
        'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    ];
    let st = glsp::str();
    let mut i = arg.abs() as usize;
    while i > 0 {
        st.push_start(&chars[i % radix]).ok();
        i /= radix;
    }

    if st.len() == 0 {
        st.push_start(&'0').ok();
    }

    if arg < 0 {
        st.push_start(&'-').ok();
    }

    Ok(st)
}

fn flo_to_str(arg: f32, decimal_places: Option<usize>) -> Root<Str> {
    let mut buf = SmallVec::<[u8; 128]>::new();

    if let Some(decimal_places) = decimal_places {
        write!(buf, "{:.*}", decimal_places, arg).unwrap();
    } else {
        write!(buf, "{}", arg).unwrap();
    }

    glsp::str_from_rust_str(str::from_utf8(&buf[..]).unwrap())
}

fn valid_sym_charp(ch: char) -> bool {
    glsp::is_valid_sym_char(ch)
}

fn valid_sym_strp(st: &str) -> bool {
    glsp::is_valid_sym(st)
}

fn representable_sym_strp(st: &str) -> bool {
    glsp::is_representable_sym(st)
}

fn samep(args: Rest<Val>) -> GResult<bool> {
    ensure!(
        args.len() >= 2,
        "expected at least 2 args, but received {}",
        args.len()
    );
    Ok((0..args.len() - 1).all(|i| args[i].same(&args[i + 1])))
}

fn eqp(args: Rest<Val>) -> GResult<bool> {
    ensure!(
        args.len() >= 2,
        "expected at least 2 args, but received {}",
        args.len()
    );

    for i in 0..args.len() - 1 {
        if !args[i].try_eq(&args[i + 1])? {
            return Ok(false);
        }
    }

    Ok(true)
}

fn keys_eqvp(args: Rest<Val>) -> GResult<bool> {
    ensure!(
        args.len() >= 2,
        "expected at least 2 args, but received {}",
        args.len()
    );
    Ok((0..args.len() - 1).all(|i| args[i].keys_eqv(&args[i + 1])))
}

fn num_eq_anyp(first: Val, rest: Rest<Val>) -> GResult<bool> {
    ensure!(
        rest.iter()
            .chain(once(&first))
            .all(|val| { val.is_int() || val.is_flo() || val.is_char() }),
        "non-number passed to ==any?"
    );
    Ok(rest.iter().any(|rest_val| first.num_eq(rest_val).unwrap()))
}

fn same_anyp(first: Val, rest: Rest<Val>) -> bool {
    rest.iter().any(|rest_val| first.same(rest_val))
}

fn eq_anyp(first: Val, rest: Rest<Val>) -> GResult<bool> {
    for rest_val in &rest {
        if first.try_eq(rest_val)? {
            return Ok(true);
        }
    }
    Ok(false)
}

fn global(name: Sym) -> GResult<Val> {
    glsp::global(name)
}

fn set_global(name: Sym, new_value: Val) -> GResult<()> {
    glsp::set_global(name, &new_value)
}

fn global_opt(name: Sym) -> GResult<Option<Val>> {
    if glsp::has_global(name)? {
        Ok(Some(glsp::global(name)?))
    } else {
        Ok(None)
    }
}

fn set_global_opt(name: Sym, new_value: Val) -> GResult<()> {
    if glsp::has_global(name)? {
        glsp::set_global(name, &new_value)?;
    }

    Ok(())
}

fn freeze_global(name: Sym) -> GResult<()> {
    glsp::freeze_global(name)
}

fn has_global(name: Sym) -> GResult<bool> {
    glsp::has_global(name)
}

fn bind_global(name: Sym, init: Option<Val>) -> GResult<()> {
    glsp::bind_global(name, init.unwrap_or(Val::Nil))
}

fn del_global(name: Sym) -> GResult<()> {
    glsp::del_global(name)
}

fn get_macro(name: Sym) -> GResult<Expander> {
    glsp::get_macro(name)
}

fn set_macro(name: Sym, mac: Expander) -> GResult<()> {
    glsp::set_macro(name, mac)
}

fn macro_opt(name: Sym) -> GResult<Option<Expander>> {
    if glsp::has_macro(name)? {
        Ok(Some(glsp::get_macro(name)?))
    } else {
        Ok(None)
    }
}

fn set_macro_opt(name: Sym, mac: Expander) -> GResult<()> {
    if glsp::has_macro(name)? {
        glsp::set_macro(name, mac)?;
    }

    Ok(())
}

fn has_macro(name: Sym) -> GResult<bool> {
    glsp::has_macro(name)
}

fn bind_macro(name: Sym, init: Expander) -> GResult<()> {
    glsp::bind_macro(name, init)
}

fn del_macro(name: Sym) -> GResult<()> {
    glsp::del_macro(name)
}

fn expand(val: Val, env_mode: Option<EnvMode>) -> GResult<Val> {
    glsp::expand(&val, env_mode)
}

fn expand_multi(vals: Vec<Val>, env_mode: Option<EnvMode>) -> GResult<Vec<Val>> {
    glsp::expand_multi(&vals[..], env_mode)
}

fn expand_1(arg: Val, expander: Option<Expander>, env_mode: Option<EnvMode>) -> GResult<Root<Arr>> {
    match glsp::expand_1(&arg, expander, env_mode)? {
        Expansion::ExpandedTo(val) => Ok(arr![EXPANDED_TO_SYM, val]),
        Expansion::MacroNoOp => Ok(arr![MACRO_NO_OP_SYM, arg]),
        Expansion::NotAMacro => Ok(arr![NOT_A_MACRO_SYM, arg]),
    }
}

fn macro_no_op() -> GResult<()> {
    macro_no_op!()
}

fn fn_name(arg: Val) -> GResult<Option<Sym>> {
    match arg {
        Val::GFn(ref gfn) => Ok(gfn.name()),
        Val::RFn(ref rfn) => Ok(rfn.name()),
        _ => bail!("expected a function, received {}", arg.a_type_name()),
    }
}

fn fn_yieldsp(gfn: Root<GFn>) -> bool {
    gfn.yields()
}

fn arg_limits(callable: Callable) -> (usize, Option<usize>) {
    callable.arg_limits()
}

fn min_args(callable: Callable) -> usize {
    callable.min_args()
}

fn max_args(callable: Callable) -> Option<usize> {
    callable.max_args()
}

fn coro_state(coro: Root<Coro>) -> Sym {
    match coro.state() {
        CoroState::Newborn => NEWBORN_SYM,
        CoroState::Running => RUNNING_SYM,
        CoroState::Paused => PAUSED_SYM,
        CoroState::Finished => FINISHED_SYM,
        CoroState::Poisoned => POISONED_SYM,
    }
}

fn coro_run(coro: Root<Coro>, arg: Option<Val>) -> GResult<Val> {
    glsp::coro_run(&coro, arg)
}

fn coro_finish(coro: Root<Coro>) -> GResult<()> {
    glsp::coro_finish(&coro)
}

fn gc() {
    glsp::gc();
}

fn gc_value(name: Sym) -> GResult<Val> {
    Ok(match name {
        RATIO_SYM => Val::Flo(glsp::gc_ratio()),
        MIN_RATIO_SYM => Val::Flo(GC_MIN_RATIO),
        DEFAULT_RATIO_SYM => Val::Flo(GC_DEFAULT_RATIO),
        YOUNG_BYTES_SYM => Val::Int(glsp::gc_young_bytes() as i32),
        OLD_BYTES_SYM => Val::Int(glsp::gc_old_bytes() as i32),
        GHOST_BYTES_SYM => Val::Int(glsp::gc_ghost_bytes() as i32),
        name => bail!("unrecognized gc-value {}", name),
    })
}

fn set_gc_value(name: Sym, to_set: Val) -> GResult<()> {
    match name {
        RATIO_SYM => match to_set {
            Val::Flo(ratio) => glsp::gc_set_ratio(ratio),
            to_set => bail!("invalid ratio {}", to_set),
        },
        name => bail!("unrecognized gc-value {}", name),
    }

    Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
fn time() -> f32 {
    super::time()
}

fn unix_time() -> String {
    UNIX_EPOCH.elapsed().unwrap().as_secs().to_string()
}

fn sleep(secs: Num) -> GResult<()> {
    super::sleep(secs.into_f32())
}

fn bail(args: Rest<Val>) -> GResult<()> {
    match args.len() {
        0 => bail!("(bail) was invoked"),
        1 => bail!(args[0].clone()),
        _ => {
            let mut builder = String::new();
            super::collections::build_msg(&mut builder, &args[..], true).ok();
            bail!("{}", builder)
        }
    }
}

fn try_call(mode: Val, callee: Val, args: Rest<Val>) -> GResult<Root<Arr>> {
    let is_verbose = match mode {
        Val::Sym(BRIEF_SYM) => false,
        Val::Sym(VERBOSE_SYM) => true,
        _ => {
            return Ok(arr![
                ERR_SYM,
                str!("expected 'brief or 'verbose, received {}", mode)
            ])
        }
    };

    let callable = if callee.is_callable() {
        Callable::from_val(&callee).unwrap()
    } else if is_verbose {
        return Ok(arr![
            ERR_SYM,
            str!("expected a Callable, received {}", callee),
            glsp::stack_trace()
        ]);
    } else {
        return Ok(arr![
            ERR_SYM,
            str!("expected a Callable, received {}", callee)
        ]);
    };

    match glsp::try_call(is_verbose, &callable, &args) {
        Ok(result) => Ok(arr![OK_SYM, result]),
        Err(err) => {
            //we allow (macro-no-op) errors to bubble through (try) and (try-verbose)
            if err.is_macro_no_op() {
                Err(err)
            } else if is_verbose {
                Ok(arr![ERR_SYM, err.val(), err.stack_trace().unwrap()])
            } else {
                Ok(arr![ERR_SYM, err.val()])
            }
        }
    }
}

fn stack_trace() -> String {
    glsp::stack_trace()
}

fn file_location() -> Option<String> {
    glsp::file_location()
}

fn dump_form(arg: Val) -> GResult<()> {
    eprn!("{}", glsp::dump_form(&arg)?);
    Ok(())
}

fn dump_fn(gfn: Root<GFn>) -> GResult<()> {
    eprn!("{}", glsp::dump_fn(&gfn)?);
    Ok(())
}

fn dump_macro(sym: Sym) -> GResult<()> {
    eprn!("{}", glsp::dump_macro(sym)?);
    Ok(())
}

fn not(arg: Val) -> bool {
    match arg {
        Val::Nil => true,
        Val::Bool(b) => !b,
        _ => false,
    }
}

fn gensym(tag: Option<Val>) -> GResult<Sym> {
    match tag {
        Some(Val::Sym(tag)) => glsp::gensym_with_tag(&tag.name()),
        Some(Val::Str(tag)) => {
            let mut bytes = SmallVec::<[u8; 128]>::new();
            write!(&mut bytes, "{}", tag).unwrap();

            glsp::gensym_with_tag(&str::from_utf8(&bytes[..]).unwrap())
        }
        Some(val) => bail!(
            "expected a string or symbol, received {}",
            val.a_type_name()
        ),
        None => Ok(glsp::gensym()),
    }
}

fn freedp(rdata: Root<RData>) -> bool {
    rdata.is_freed()
}

fn freeze(arg: Val) -> Val {
    arg.freeze();
    arg
}

fn deep_freeze(arg: Val) -> Val {
    arg.deep_freeze();
    arg
}

/*
fn command_line_args() -> Root<Arr> {
    //we use OsString here to avoid panicking if some kind of non-UTF string is passed on the
    //command line, e.g. as the filepath on a non-English system
    let os_args: Vec<OsString> = env::args_os().collect();
    let arr = glsp::arr();

    if os_args.len() < 1 {
        //we guarantee that the first arg is present
        arr.push(&Val::Str(glsp::str())).ok();
    } else {
        //convert each native arg into a str, replacing it with an empty str if it's invalid
        for os_arg in os_args.into_iter() {
            let st = glsp::str_from_rust_str(&os_arg.into_string().unwrap_or("".into()));
            arr.push(&Val::Str(st)).ok();
        }
    }

    arr
}
*/

fn clone(arg: Val) -> GResult<Val> {
    arg.shallow_clone()
}

fn deep_clone(arg: Val) -> GResult<Val> {
    arg.deep_clone()
}

fn load_str(text: &str) -> GResult<Val> {
    glsp::load_str(text)
}

fn eval(val: Val, env_mode: Option<EnvMode>) -> GResult<Val> {
    glsp::eval(&val, env_mode)
}

fn eval_multi(vals: Vec<Val>, env_mode: Option<EnvMode>) -> GResult<Val> {
    glsp::eval_multi(&vals[..], env_mode)
}

fn load(filename: String) -> GResult<Val> {
    glsp::load(&filename)
}

fn require(filename: String) -> GResult<Val> {
    glsp::require(&filename)
}

fn no_op(_: Rest<Val>) {
    //deliberate no-op
}

fn identity(arg: Val) -> Val {
    arg
}
