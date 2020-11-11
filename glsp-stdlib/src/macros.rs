use glsp::{
	arr, Arr, bail, bail_at, Callable, DequeAccess, DequeOps, 
	ensure, EnvMode, error, FromVal, GResult, Lib, macro_no_op, 
	rfn, Root, Span, Sym, stock_syms::*, str, Val
};
use glsp_proc_macros::{backquote};
use smallvec::{SmallVec};
use std::{fs};
use std::collections::{HashMap, hash_map::Entry::{Occupied, Vacant}, HashSet};
use std::default::{Default};
use std::iter::FromIterator;
use super::{Std};
use super::pat::{
	AssignStrategy, Matcher, MismatchStrategy, Pat, pat_from_forms, PlaceStrategy, SetStrategy
};

pub fn init(sandboxed: bool) -> GResult<()> {
	if !sandboxed {
		glsp::bind_rfn_macro("include", rfn!(include))?;
	}
	
	glsp::bind_rfn_macro("+", rfn!(add))?;
	glsp::bind_rfn_macro("-", rfn!(sub))?;
	glsp::bind_rfn_macro("*", rfn!(mul))?;
	glsp::bind_rfn_macro("/", rfn!(div))?;
	glsp::bind_rfn_macro("min", rfn!(min))?;
	glsp::bind_rfn_macro("max", rfn!(max))?;
	glsp::bind_rfn_macro("bitand", rfn!(bitand))?;
	glsp::bind_rfn_macro("bitor", rfn!(bitor))?;
	glsp::bind_rfn_macro("bitxor", rfn!(bitxor))?;

	glsp::bind_rfn_macro("def", rfn!(def))?;
	glsp::bind_rfn_macro("defn", rfn!(defn))?;
	glsp::bind_rfn_macro("defmacro", rfn!(defmacro))?;
	glsp::bind_rfn_macro("with-global", rfn!(with_global))?;

	glsp::bind_rfn_macro("global", rfn!(global))?;
	glsp::bind_rfn_macro("global=", rfn!(set_global))?;
	glsp::bind_rfn_macro("macro", rfn!(get_macro))?;
	glsp::bind_rfn_macro("macro=", rfn!(set_macro))?;

	glsp::bind_rfn_macro("access", rfn!(access))?;
	glsp::bind_rfn_macro("access=", rfn!(set_access))?;
	glsp::bind_rfn_macro("del!", rfn!(del))?;
	glsp::bind_rfn_macro("remove!", rfn!(remove))?;

	glsp::bind_rfn_macro("call-met", rfn!(call_met))?;

	glsp::bind_rfn_macro("ensure", rfn!(ensure))?;
	glsp::bind_rfn_macro("dbg", rfn!(dbg))?;
	glsp::bind_rfn_macro("todo", rfn!(todo))?;
	glsp::bind_rfn_macro("try", rfn!(try_))?;
	glsp::bind_rfn_macro("try-verbose", rfn!(try_verbose))?;

	glsp::bind_rfn_macro("when", rfn!(when))?;
	glsp::bind_rfn_macro("unless", rfn!(unless))?;
	glsp::bind_rfn_macro("while", rfn!(while_))?;
	glsp::bind_rfn_macro("until", rfn!(until))?;
	glsp::bind_rfn_macro("cond", rfn!(cond))?;
	glsp::bind_rfn_macro("and", rfn!(and))?;
	glsp::bind_rfn_macro("or", rfn!(or))?;
	glsp::bind_rfn_macro("do-0", rfn!(do_0))?;

	glsp::bind_rfn_macro("cond==", rfn!(cond_num_eq))?;
	glsp::bind_rfn_macro("cond-same?", rfn!(cond_same))?;
	glsp::bind_rfn_macro("cond-eq?", rfn!(cond_eq))?;

	glsp::bind_rfn_macro("backquote", rfn!(backquote))?;

	glsp::bind_rfn_macro("let", rfn!(let_))?;
	glsp::bind_rfn_macro("let-fn", rfn!(let_fn))?;

	glsp::bind_rfn_macro("match", rfn!(match_))?;
	glsp::bind_rfn_macro("matches?", rfn!(matchesp))?;
	glsp::bind_rfn_macro("when-let", rfn!(when_let))?;

	glsp::bind_rfn_macro("fn", rfn!(fn_))?;
	glsp::bind_rfn_macro("%met-fn", rfn!(met_fn))?;
	glsp::bind_rfn_macro("fn0", rfn!(fn0))?;
	glsp::bind_rfn_macro("fn1", rfn!(fn1))?;

	glsp::bind_rfn_macro("->", rfn!(arrow_first))?;
	glsp::bind_rfn_macro("->>", rfn!(arrow_last))?;

	glsp::bind_rfn_macro("?", rfn!(question_mark))?;

	glsp::bind_rfn_macro("tab", rfn!(tab))?;

	glsp::bind_rfn_macro("for", rfn!(for_))?;
	glsp::bind_rfn_macro("forn", rfn!(forn))?;
	glsp::bind_rfn_macro("forni", rfn!(forni))?;
	glsp::bind_rfn_macro("break", rfn!(break_))?;
	glsp::bind_rfn_macro("continue", rfn!(continue_))?;
	glsp::bind_rfn_macro("loop", rfn!(loop_))?;
	glsp::bind_rfn_macro("yield-from", rfn!(yield_from))?;

	//setters and in-place mutation
	glsp::bind_rfn_macro("defplace", rfn!(defplace))?;
	glsp::bind_rfn("bind-place!", rfn!(bind_place))?;
	glsp::bind_rfn_macro("=", rfn!(set))?;
	
	static SETTERS: [(&str, &str, Option<bool>); 7] = [
		("access", "access=", None),
		("access-opt", "access-opt=", None),
		("access-slice", "access-slice=", None),
		("global", "global=", None),
		("macro", "macro=", None),
		("gc-value", "gc-value=", None),
		("atsign", "atsign=", Some(false))
	];

	static OPT_SETTERS: [(&str, &str, Option<bool>); 1] = [
		("atsign", "atsign-opt=", Some(false))
	];

	let mut std = Std::borrow_mut();

	for (accessor, setter, memoize_args) in &SETTERS {
		let memoize_args = memoize_args.unwrap_or(true);
		std.setters.insert(glsp::sym(accessor)?, (glsp::sym(setter)?, memoize_args));
	}

	for (accessor, setter, memoize_args) in &OPT_SETTERS {
		let memoize_args = memoize_args.unwrap_or(true);
		std.opt_setters.insert(glsp::sym(accessor)?, (glsp::sym(setter)?, memoize_args));
	}

	glsp::bind_rfn_macro("inc!", rfn!(inc_assign))?;
	glsp::bind_rfn_macro("dec!", rfn!(dec_assign))?;
	glsp::bind_rfn_macro("mul!", rfn!(mul_assign))?;
	glsp::bind_rfn_macro("div!", rfn!(div_assign))?;
	glsp::bind_rfn_macro("div-euclid!", rfn!(div_euclid_assign))?;
	glsp::bind_rfn_macro("rem!", rfn!(rem_assign))?;
	glsp::bind_rfn_macro("rem-euclid!", rfn!(rem_euclid_assign))?;
	glsp::bind_rfn_macro("abs!", rfn!(abs_assign))?;
	glsp::bind_rfn_macro("neg!", rfn!(neg_assign))?;
	glsp::bind_rfn_macro("seek!", rfn!(seek_assign))?;
	glsp::bind_rfn_macro("antiseek!", rfn!(antiseek_assign))?;
	glsp::bind_rfn_macro("clamp!", rfn!(clamp_assign))?;
	glsp::bind_rfn_macro("swap!", rfn!(swap_assign))?;

	Ok(())
}

fn is_splay_form(val: &Val) -> bool {
	match val {
		Val::Arr(arr) => arr.len() >= 1 && arr.get::<Val>(0).unwrap() == Val::Sym(SPLAY_SYM),
		_ => false
	}
}

/*
"binarization" converts certain calls into a chain of nested two-argument calls, so that they 
will be compiled to OpSomething instructions rather than the more-expensive CallX instructions

"constant folding" detects when a numeric operation's arguments are all constants, and if so
attempts to perform the operation at compile time. on success, the result is emitted in place
of the form itself.

(+ a b c d) -> (+ (+ (+ a b) c) d)
(+ a) -> (+ 0 a)
(+ 1 2) -> 3

some special cases:
- calls with at least one (splay ...) argument are not binarized
- unary negation is not binarized, because (- 0 0.0) is not the same as (- 0.0)
*/

fn binarize(
	callee: Sym, 
	identity_val: Option<Val>, 
	replaceable: bool, 
	args: &[Val]
) -> Option<Val> {

	//calls with at least one (splay ...) argument are not binarized
	for arg in args {
		if is_splay_form(arg) {
			return None
		}
	}

	match args.len() {
		0 => {
			//for example, (+) can be replaced with the literal 1
			if replaceable {
				Some(identity_val.unwrap())
			} else {
				None
			}
		}

		1 => {
			//for example, (- 8) can be replaced with (- 0 8)
			if let Some(identity_val) = identity_val {
				Some(Val::Arr(arr![callee, identity_val, &args[0]]))
			} else {
				None
			}
		}

		2 => {
			None
		}

		_ => {
			let result = arr![..args];
			while result.len() > 2 {
				let first: Val = result.pop_start().unwrap();
				let second: Val = result.pop_start().unwrap();
				result.push_start(arr![callee, first, second]).unwrap();
			}

			result.push_start(callee).unwrap();

			Some(Val::Arr(result))
		}
	}
}

/*
fn ternarize_setter(setter: Sym, accessor: Sym, raw_args: &[Val]) -> Option<Val> {
	if raw_args.len() <= 3 {
		return None
	}

	//calls with at least one (splay ...) argument are not ternarized
	for arg in raw_args {
		if let Val::Arr(ref arr) = *arg {
			if arr.len() >= 1 && arr.get::<Val>(0).unwrap() == Val::Sym(SPLAY_SYM) {
				return None
			}
		}
	}

	let args = arr![..raw_args];
	let new_val = args.pop::<Val>().unwrap();
	let last_index = args.pop::<Val>().unwrap();

	Some(Val::Arr(arr![setter, arr![accessor, ..args], last_index, new_val]))
}
*/

fn constant_fold(callee: Sym, args: &[Val]) -> Option<Val> {
	if !args.iter().all(|arg| arg.is_num()) {
		return None
	}

	if !glsp::has_global(callee).unwrap() {
		return None
	}

	let callable = match glsp::global::<_, Callable>(callee) {
		Ok(callable) => callable,
		_ => return None
	};

	match glsp::try_call(false, &callable, args) {
		Ok(val) => Some(val),
		Err(_) => None
	}
}

macro_rules! arithmetic_macro {
	($name:ident, $callee:expr, $identity_val:expr, $replaceable:expr) => (
		fn $name(args: &[Val]) -> GResult<Val> {
			if let Some(result) = constant_fold($callee, args) {
				return Ok(result)
			}

			if let Some(result) = binarize($callee, $identity_val, $replaceable, args) {
				return Ok(result)
			}

			macro_no_op!()
		}
	);
}

arithmetic_macro!(add, ADD_SYM, Some(Val::Int(0)), true);

fn sub(args: &[Val]) -> GResult<Val> {
	if let Some(result) = constant_fold(SUB_SYM, args) {
		return Ok(result)
	}

	if let Some(result) = binarize(SUB_SYM, None, false, args) {
		return Ok(result)
	}

	macro_no_op!()
}

arithmetic_macro!(mul, MUL_SYM, Some(Val::Int(1)), true);
arithmetic_macro!(div, DIV_SYM, Some(Val::Int(1)), false);
arithmetic_macro!(min, MIN_SYM, None, false);
arithmetic_macro!(max, MAX_SYM, None, false);
arithmetic_macro!(bitand, BITAND_SYM, Some(Val::Int(-1)), true);
arithmetic_macro!(bitor, BITOR_SYM, Some(Val::Int(0)), true);
arithmetic_macro!(bitxor, BITXOR_SYM, Some(Val::Int(0)), true);

fn include(path: &str) -> GResult<Val> {
	let content_str = match fs::read_to_string(&path) {
		Ok(content_str) => content_str,
		Err(err) => {
			let msg = error!("unable to read file '{}'", path);
			return Err(msg.with_source(err))
		}
	};

	let mut vals = glsp::parse_all(&content_str, Some(path))?;

	if vals.len() == 1 {
		Ok(vals.pop().unwrap())
	} else {
		Ok(Val::Arr(arr![SPLICE_SYM, ..vals]))
	}
}

fn def(args: &[Val]) -> GResult<Val> {
	match expand_let_like(DEF_SYM, PlaceStrategy::Global, args)? {
		Some(expanded) => Ok(expanded),
		None => {
			match args.len() {
				0 => Ok(Val::Nil),
				1 | 2 => {
					let name = Sym::from_val(&args[0])?;
					let init = args.get(1).cloned().unwrap_or(Val::Nil);

					Ok(backquote!("(bind-global! '~name ~init)"))
				}
				_ => unreachable!()
			}
		}
	}
}

fn defn(name: Sym, params: Root<Arr>, body: &[Val]) -> Val {
	backquote!(r#"
		(bind-global! '~name (fn &name ~name ~params ~..body))
	"#)
}

fn defmacro(name: Sym, params: Root<Arr>, body: &[Val]) -> Val {
	backquote!(r#"
		(bind-macro! '~name (fn &name ~name ~params ~..body))
	"#)
}

fn with_global(args: &[Val]) -> GResult<Val> {
	match expand_let_like(WITH_GLOBAL_SYM, PlaceStrategy::Global, args)? {
		Some(expanded) => Ok(expanded),
		None => {
			match args.len() {
				0 => Ok(Val::Nil),
				1 | 2 => {
					let name = Sym::from_val(&args[0])?;
					let init = args.get(1).cloned().unwrap_or(Val::Nil);

					Ok(backquote!(r#"
						(splice
						  (let inner_value# ~init)
						  (let outer_value# (global '~name))
						  (global= '~name inner_value#)
						  (defer
						    (global= '~name outer_value#))
						  (defer-yield
						    (do
						      (= inner_value# (global '~name))
						      (global= '~name outer_value#))
						    (do
						      (= outer_value# (global '~name))
						      (global= '~name inner_value#)))
						  #n)
					"#))
				}
				_ => unreachable!()
			}
		}
	}
}

//we generalise handling of (? x) optional arguments, and `x : y` slice arguments, to several
//different macro-rfns (global, global=, access, access=, remove!, del!, call-met). they
//expand to their something-opt or something-slice variants, respectively.

fn unwrap_question_mark(val: Val) -> GResult<Option<Val>> {
	match val {
		Val::Arr(arr) => {
			if arr.len() >= 1 && arr.get::<Val>(0).unwrap() == Val::Sym(QUESTION_MARK_SYM) {
				ensure!(arr.len() == 2, "? expects 1 argument, but received {}", arr.len() - 1);

				let arg = arr.get::<Val>(1).unwrap();
				ensure!(!is_splay_form(&arg), "the argument to ? cannot be splayed");

				Ok(Some(arg))
			} else {
				Ok(None)
			}
		}
		_ => Ok(None)
	}
}

fn unwrap_slice_args(rest: &[Val]) -> GResult<Option<(Val, Val)>> {
	let to_from = match rest {
		&[Val::Sym(COLON_SYM)] => Some((Val::Nil, Val::Nil)),
		&[ref from, Val::Sym(COLON_SYM)] => Some((from.clone(), Val::Nil)),
		&[Val::Sym(COLON_SYM), ref to] => Some((Val::Nil, to.clone())),
		&[ref from, Val::Sym(COLON_SYM), ref to] => Some((from.clone(), to.clone())),
		_ => {
			ensure!(!rest.contains(&Val::Sym(COLON_SYM)), "invalid slice arguments");
			None
		}
	};

	if let Some((to, from)) = &to_from {
		ensure!(!is_splay_form(to) && !is_splay_form(from), "slice arguments cannot be splayed");
	}

	Ok(to_from)
}

fn global(name: Val) -> GResult<Val> {
	if let Some(opt_name) = unwrap_question_mark(name)? {
		return Ok(backquote!("(global-opt ~opt_name)"))
	}

	macro_no_op!()
}

fn set_global(name: Val, new_val: Val) -> GResult<Val> {
	if let Some(opt_name) = unwrap_question_mark(name)? {
		return Ok(backquote!("(global-opt= ~opt_name ~new_val)"))
	}

	macro_no_op!()
}

fn get_macro(name: Val) -> GResult<Val> {
	if let Some(opt_name) = unwrap_question_mark(name)? {
		return Ok(backquote!("(macro-opt ~opt_name)"))
	}

	macro_no_op!()
}

fn set_macro(name: Val, new_val: Val) -> GResult<Val> {
	if let Some(opt_name) = unwrap_question_mark(name)? {
		return Ok(backquote!("(macro-opt= ~opt_name ~new_val)"))
	}

	macro_no_op!()
}

fn access(coll: Val, rest: &[Val]) -> GResult<Val> {
	if let Some((from, to)) = unwrap_slice_args(rest)? {
		return Ok(backquote!("(access-slice ~coll ~from ~to)"))
	}

	if rest.len() >= 1 {
		if let Some(opt_index) = unwrap_question_mark(rest[0].clone())? {
			ensure!(rest.len() == 1, "too many arguments to [coll (? idx)] form");
			return Ok(backquote!("(access-opt ~coll ~opt_index)"))
		}
	}

	macro_no_op!()
}

fn set_access(coll: Val, rest: &[Val]) -> GResult<Val> {
	if rest.len() == 0 {
		macro_no_op!()
	}

	let new_val = rest.last().unwrap().clone();
	let args = &rest[..rest.len() - 1];

	if let Some((from, to)) = unwrap_slice_args(args)? {
		return Ok(backquote!("(access-slice= ~coll ~from ~to ~new_val)"))
	}

	if args.len() >= 1 {
		if let Some(opt_index) = unwrap_question_mark(args[0].clone())? {
			ensure!(args.len() == 1, "too many arguments to (= [coll (? idx)] new-val) form");
			return Ok(backquote!("(access-opt= ~coll ~opt_index ~new_val)"))
		}
	}

	macro_no_op!()
}

fn remove(coll: Val, rest: &[Val]) -> GResult<Val> {
	if let Some((from, to)) = unwrap_slice_args(rest)? {
		return Ok(backquote!("(remove-slice! ~coll ~from ~to)"))
	}

	ensure!(rest.len() == 1, "argument mismatch: expected 2 arguments, \
	        received {}", rest.len() + 1);

	if let Some(opt_index) = unwrap_question_mark(rest[0].clone())? {
		return Ok(backquote!("(remove-opt! ~coll ~opt_index)"))
	}

	macro_no_op!()
}

fn del(coll: Val, rest: &[Val]) -> GResult<Val> {
	if let Some((from, to)) = unwrap_slice_args(rest)? {
		return Ok(backquote!("(del-slice! ~coll ~from ~to)"))
	}

	ensure!(rest.len() == 1, "argument mismatch: expected 2 arguments, \
	        received {}", rest.len());

	if let Some(opt_index) = unwrap_question_mark(rest[0].clone())? {
		return Ok(backquote!("(del-opt! ~coll ~opt_index)"))
	}

	macro_no_op!()
}

fn call_met(args: &[Val]) -> GResult<Val> {
	if args.len() == 0 {
		macro_no_op!()
	}

	if let Some(opt_met_name) = unwrap_question_mark(args[0].clone())? {
		let rest = &args[1..];
		Ok(backquote!("(call-met-opt ~opt_met_name ~..rest)"))
	} else {
		macro_no_op!()
	}
}

fn ensure(test: Val, error_args: &[Val]) -> Val {
	if error_args.len() == 0 {
		let message = str!("(ensure {}) failed", &test);
		backquote!("(if ~test #n (bail ~message))")
	} else {
		backquote!("(if ~test #n (bail ~..error_args))")
	}
}

//todo: support splayed arguments to (dbg)
fn dbg(forms: &[Val]) -> GResult<Root<Arr>> {
	let result = arr![DO_SYM];
	for form in forms {
		let form_name = glsp::gensym();
		let stringified_form = str!["{:?}", form];

		let clause: Val = backquote!(r#"
			(do
			  (let ~form_name ~form)
			  (eprn (if (str? ~form_name)
			  	(str "[" (file-location) "] " ~stringified_form " = \"" ~form_name "\"")
			  	(str "[" (file-location) "] " ~stringified_form " = " ~form_name))))
		"#);
		result.push(clause)?;
	}

	Ok(result)
}

fn todo(forms: &[Val]) -> Root<Arr> {
	if forms.len() == 0 {
		backquote!(r#"(bail "not yet implemented")"#)
	} else {
		backquote!(r#"(bail "not yet implemented: " ~..forms)"#)
	}
}

fn try_(body: &[Val]) -> Root<Arr> {
	backquote!("(try-call 'brief (fn () ~..body))")
}

fn try_verbose(body: &[Val]) -> Root<Arr> {
	backquote!("(try-call 'verbose (fn () ~..body))")
}

fn when(cond_clause: Val, rest: &[Val]) -> Root<Arr> {
	let then_clause = match rest.len() {
		0 => Val::Nil,
		1 => rest[0].clone(),
		_ => backquote!("(do ~..rest)")
	};

	backquote!("(if ~cond_clause ~then_clause #n)")
}

fn unless(cond_clause: Val, rest: &[Val]) -> Root<Arr> {
	let then_clause = match rest.len() {
		0 => Val::Nil,
		1 => rest[0].clone(),
		_ => backquote!("(do ~..rest)")
	};

	backquote!("(if ~cond_clause #n ~then_clause)")
}

fn while_(cond_clause: Val, body: &[Val]) -> Root<Arr> {
	backquote!(r#"
		(block LOOP
		  (if ~cond_clause
		    (do
		      ~..body
		      (restart-block LOOP))
		    #n))
	"#)
}

fn until(cond_clause: Val, body: &[Val]) -> Root<Arr> {
	backquote!(r#"
		(block LOOP
		  (if ~cond_clause
		    #n
		    (do
		      ~..body
		      (restart-block LOOP))))
	"#)
}

fn cond(clauses: &[Root<Arr>]) -> GResult<Val> {
	let mut result = Val::Nil;

	for (rev_i, clause) in clauses.iter().rev().enumerate() {
		ensure!(clause.len() >= 1, "empty `cond` clause");

		let predicate = clause.get::<Val>(0)?;
		let body = glsp::arr_from_iter(clause.iter().skip(1))?;

		if predicate == Val::Sym(ELSE_SYM) {
			ensure!(rev_i == 0, "`else` must be the last clause in the `cond` form");
			result = backquote!("(do ~..body)");
		} else {
			if body.len() == 0 {
				//clauses which are just (predicate) with no body should evaluate to the result of
				//the predicate when it's non-nil and non-false, for consistency with other lisps.
				result = backquote!(r#"
					(do
					  (let predicate-name# ~predicate)
					  (if predicate-name# predicate-name# ~result))
				"#);
			} else {
				result = backquote!("(if ~predicate (do ~..body) ~result)");
			}
		}
	}

	Ok(result)
}

fn and(forms: &[Val]) -> Val {
	if forms.len() == 0 {
		Val::Bool(true)
	} else {
		let mut result = forms.last().unwrap().clone();

		for form in forms.iter().rev().skip(1) {
			result = backquote!("(if ~form ~result #f)");
		}

		result
	}
}

fn or(forms: &[Val]) -> GResult<Val> {
	if forms.len() == 0 {
		Ok(Val::Bool(false))
	} else {
		let mut result = forms.last().unwrap().clone();

		for form in forms.iter().rev().skip(1) {
			result = backquote!(r#"
				(do
				  (let form-name# ~form)
				  (if form-name# form-name# ~result))
			"#);
		}

		Ok(result)
	}
}

fn do_0(first: Val, rest: &[Val]) -> Val {
	backquote!(r#"
		(do
		  (let first-name# ~first)
		  ~..rest
		  first-name#)
	"#)
}

fn cond_num_eq(test_form: Val, clauses: &[Val]) -> GResult<Val> {
	cond_with_comparator(NUM_EQ_SYM, test_form, clauses)
}

fn cond_same(test_form: Val, clauses: &[Val]) -> GResult<Val> {
	cond_with_comparator(SAMEP_SYM, test_form, clauses)
}

fn cond_eq(test_form: Val, clauses: &[Val]) -> GResult<Val> {
	cond_with_comparator(EQP_SYM, test_form, clauses)
}

fn cond_with_comparator(comparator: Sym, test_form: Val, clauses: &[Val]) -> GResult<Val> {
	let test_name = glsp::gensym();

	let cond_form = glsp::arr_with_capacity(clauses.len() + 1);
	cond_form.push(COND_SYM)?;

	for clause in clauses {
		ensure!(clause.is_arr(), "invalid cond clause {}", &clause);
		let clause = Root::<Arr>::from_val(&clause)?;
		ensure!(clause.len() >= 1, "invalid cond clause {}", &clause);
		let first = clause.get::<Val>(0)?;

		if first == Val::Sym(ELSE_SYM) {
			cond_form.push(clause)?;
		} else if first == Val::Sym(ANY_OF_SYM) {
			ensure!(clause.len() >= 2 && clause.get::<Val>(1)?.is_arr(), "invalid any-of clause");
			let candidates = clause.get::<Root<Arr>>(1)?;

			let or_form = glsp::arr_with_capacity(candidates.len() + 1);
			or_form.push(OR_SYM)?;

			for candidate in candidates.iter() {
				or_form.push::<Val>(backquote!("(~comparator ~candidate ~test_name)"))?;
			}

			let rest = glsp::arr_from_iter(clause.iter().skip(2))?;
			cond_form.push::<Val>(backquote!("(~or_form ~..rest)"))?;
		} else {
			let rest = glsp::arr_from_iter(clause.iter().skip(1))?;
			cond_form.push::<Val>(backquote!("((~comparator ~first ~test_name) ~..rest)"))?;
		}
	}

	Ok(backquote!(r#"
		(do 
		  (let ~test_name ~test_form) 
		  ~cond_form)
	"#))
}

fn backquote(arg: Val) -> GResult<Val> {
	let mut auto_gensyms = HashMap::new();
	let val = backquote_impl(arg, &mut auto_gensyms)?;

	if auto_gensyms.len() == 0 {
		Ok(val)
	} else {
		let result = arr![DO_SYM];
		for (original_sym, local_sym) in auto_gensyms {
			let original_str = original_sym.name();
			let tag = &original_str[0 .. original_str.len() - 1];
			let let_form: Val = backquote!("(let ~local_sym (gensym ~tag))");
			result.push(let_form)?;
		}
		result.push(val)?;

		Ok(Val::Arr(result))
	}
}

fn backquote_impl(arg: Val, auto_gensyms: &mut HashMap<Sym, Sym>) -> GResult<Val> {
	//for auto-gensym, we just detect each unique sym which ends with #, emit a local variable
	//and initialize it to a gensym, and replace the # sym with the local variable's name.
	//we don't recursively match the same auto-gensym for nested backquote forms.
	match arg {
		Val::Arr(arr) if arr.len() == 0 => {
			Ok(Val::Arr(arr![]))
		}

		Val::Arr(arr) if arr.get::<Val>(0)? == Val::Sym(UNQUOTE_SYM) => {
			ensure!(arr.len() == 2, "invalid `unquote` form");
			Ok(arr.get::<Val>(1)?)
		}

		Val::Arr(arr) if arr.get::<Val>(0)? == Val::Sym(BACKQUOTE_SYM) => {
			Ok(Val::Arr(arr))
		}

		Val::Arr(arr) => {
			let result = glsp::arr_with_capacity(arr.len() + 1);

			//backquote "passes through" its Spans, making it transparent for error-reporting
			result.set_span(arr.span());

			result.push(ARR_SYM)?;
			for item in arr.iter() {
				result.push(backquote_impl(item, auto_gensyms)?)?;
			}

			Ok(Val::Arr(result))
		}

		Val::Sym(sym) if sym.name().ends_with("#") => {
			let local_name = match auto_gensyms.entry(sym) {
				Occupied(entry) => *entry.get(),
				Vacant(entry) => *entry.insert(glsp::gensym_with_tag("auto-gensym")?)
			};

			Ok(Val::Sym(local_name))
		}

		_ => {
			Ok(backquote!("(quote ~arg)"))
		}
	}
}

//the common pathway for handling patterns in (let), (def) and (with-global). either expands
//to something like (splice (let a #n) (let b init-form)), or returns None if the input is
//already simple.
fn expand_let_like(
	let_name: Sym,
	place_strategy: PlaceStrategy,
	args: &[Val]
) -> GResult<Option<Val>> {

	//parse the leading pattern, if any
	if args.len() == 0 {
		return Ok(None)
	}

	let (pat, forms_consumed) = pat_from_forms(args, false, Span::default())?;
	let mut rest_args = &args[forms_consumed..];

	//check for trivial cases
	if forms_consumed == 1 && pat.at.is_none() && pat.pred.is_none() {
		match args {
			&[] => unreachable!(),
			&[Val::Sym(UNDERSCORE_SYM)] => return Ok(Some(backquote!("(~let_name)"))),
			&[Val::Sym(UNDERSCORE_SYM), ref init_val] => {
				return Ok(Some(backquote!("(do ~init_val #n)")))
			}
			&[Val::Sym(name)] => return Ok(Some(backquote!("(~let_name ~name #n)"))),
			&[Val::Sym(_name), ref _init_val] => return Ok(None),
			&[Val::Sym(name), ref init_val, ref rest @ ..] => {
				return Ok(Some(backquote!("
					(splice
					  (~let_name ~name ~init_val)
					  (~let_name ~..rest))
				")))
			}
			_ => ()
		}
	}

	//emit (splice {pattern-handling code} (let ..rest))
	let init_form: Option<Val> = if rest_args.len() > 0 {
		let init_form = rest_args[0].clone();
		rest_args = &rest_args[1..];
		Some(init_form)
	} else {
		None
	};

	//todo: a fast path for (let [a b c] ...). the current codegen is a little bloated.
	//also check whether a special path for (let (a b c) ...) would generate nicer bytecode,
	//bearing in mind that the (arr?) check is not optional.

	//the generic path which can handle any pattern. we wrap the pattern's codegen in a (do) so 
	//that any temporary local variables don't reserve a register for any longer than necessary.
	let splice_form: Root<Arr> = backquote!("(splice)");

	let mut names = HashSet::new();
	pat.names(&mut names, false);

	for name in names {
		let to_push: Val = backquote!("(~let_name ~name #n)");
		splice_form.push(to_push)?;
	}

	if let Some(init_form) = init_form {
		let init_name = glsp::gensym();
		let do_form: Root<Arr> = backquote!("(do (let ~init_name ~init_form))");

		let set_strategy = SetStrategy(place_strategy, AssignStrategy::Set);
		pat.codegen(init_name, &do_form, set_strategy, MismatchStrategy::Bail)?;

		splice_form.push(do_form)?;
	}

	if rest_args.len() > 0 {
		let to_push: Val = backquote!("(~let_name ~..rest_args)");
		splice_form.push(to_push)?;
	}

	Ok(Some(Val::Arr(splice_form)))
}


fn let_(args: &[Val]) -> GResult<Val> {
	match expand_let_like(LET_SYM, PlaceStrategy::Local, args)? {
		Some(expanded) => Ok(expanded),
		None => {
			if args.len() == 0 {
				Ok(backquote!("(splice)"))
			} else {
				macro_no_op!()
			}
		}
	}
}

fn let_fn(name: Sym, params: Root<Arr>, body: &[Val]) -> Val {
	backquote!(r#"
		(splice
		  (let ~name #n)
		  (= ~name (fn &name ~name ~params ~..body)))
	"#)
}

fn match_(input_form: Val, clauses: &[Root<Arr>]) -> GResult<Val> {
	//(match) is implemented in terms of (when-let)
	let block_name = glsp::gensym();
	let input_name = glsp::gensym();
	let block_form: Root<Arr> = backquote!("
		(block ~block_name
		  (let ~input_name ~input_form))
	");

	for clause in clauses {
		let clause_forms = SmallVec::<[Val; 16]>::from_iter(clause.iter());

		let (_, forms_consumed) = pat_from_forms(&clause_forms[..], false, clause.span())?;
		let clause_pat = &clause_forms[..forms_consumed];
		let clause_body = &clause_forms[forms_consumed..];

		let when_let_form: Val = backquote!("
			(when-let ~..clause_pat ~input_name
			  (finish-block ~block_name (do ~..clause_body)))
		");
		block_form.push(when_let_form)?;
	}

	block_form.push(Val::Nil)?;

	Ok(Val::Arr(block_form))
}

fn matchesp(input_form: Val, pat_forms: &[Val]) -> GResult<Val> {
	//emit a (bool (block name {pattern-matching code} #t)), where the pattern-matching code 
	//mismatches with (finish-block name #n).
	let (pat, forms_consumed) = pat_from_forms(pat_forms, false, Span::default())?;
	ensure!(forms_consumed == pat_forms.len(), "unexpected end of pattern in (matches?)");

	let block_name = glsp::gensym();
	let input_name = glsp::gensym();
	let block_form: Root<Arr> = backquote!("
		(block ~block_name
		  (let ~input_name ~input_form))
	");

	pat.codegen(
		input_name,
		&block_form,
		SetStrategy(PlaceStrategy::Local, AssignStrategy::Discard),
		MismatchStrategy::FinishBlock(block_name)
	)?;

	block_form.push(true)?;

	Ok(backquote!("(bool ~block_form)"))
}

fn when_let(args: &[Val]) -> GResult<Val> {
	//emit a (block name {pattern-matching code} ..body), where the pattern-matching code 
	//mismatches with (finish-block name #n).
	let (pat, forms_consumed) = pat_from_forms(args, false, Span::default())?;
	ensure!(args.len() >= forms_consumed + 1, "(when-let) is missing an initializer");

	let init_form = args[forms_consumed].clone();
	let body = &args[forms_consumed + 1..];

	let block_name = glsp::gensym();
	let input_name = glsp::gensym();
	let block_form: Root<Arr> = backquote!("
		(block ~block_name
		  (let ~input_name ~init_form))
	");

	let mut names = HashSet::new();
	pat.names(&mut names, false);
	for name in names {
		let to_push: Val = backquote!("(let ~name #n)");
		block_form.push(to_push)?;
	}

	pat.codegen(
		input_name,
		&block_form,
		SetStrategy(PlaceStrategy::Local, AssignStrategy::Set),
		MismatchStrategy::FinishBlock(block_name)
	)?;

	if body.len() == 0 {
		block_form.push(Val::Nil)?;
	} else {
		for body_form in body {
			block_form.push(body_form)?;
		}
	}

	Ok(Val::Arr(block_form))
}

/*
(fn) is implemented in terms of (let). we have three possible strategies for how to expand 
any particular fn, say (fn (pat0 _ (? pat1 init) ..pat2) body)

Trivial: 
	(fn (pat0 gs0 (? pat1 init) ..pat2)
	  body)

Simple:
	(fn (gs0 gs1 (? gs2 init) ..gs3)
	  (let pat0 gs0)
	  (let b gs2)
	  (let c gs3)
	  body)

Complex:
	(fn &arg-limits 2 #n (..gs0)
	  (let (pat0 _ (? pat1) ..pat2) gs0)
	  body)

the Complex strategy is valid in all cases. everything else is just a (very significant!) 
performance optimization.

we currently use the Trivial strategy when all patterns are _ or symbols, all opt-param
initializers are trivial (anything which has no possibility of referring to the name of another
parameter, e.g. 5 or 'hello), and any rest param is at the end.

we use the Simple strategy when normal arguments are something other than _ or a symbol, but the
opt/rest params are still _ or symbols, and opt initializers are still trivial, and the rest param
is still at the end. it's not as register-hungry as it looks, because encoder.rs can detect when a 
local variable is renamed, and reuse its register.
*/

#[derive(Copy, Clone, PartialEq)]
enum FnStrategy {
	Trivial,
	Simple,
	Complex
}

fn fn_(args: &[Val]) -> GResult<Val> {
	fn_common(args, false)
}

/*
the (%met-fn) macro is exactly like (fn), except that @-bindings are permitted in param patterns.
`@param` is handled as though it was `param`. this saves us from needing to write a transformation 
pass to change (fn (@a @b (? @c)) ..) into (fn (a b (? c)) ...).
*/

fn met_fn(args: &[Val]) -> GResult<Val> {
	fn_common(args, true)
}

fn fn_common(args: &[Val], atsign_params: bool) -> GResult<Val> {
	//detect the params array and any flags
	let (params_i, params_arr) = {
		let mut i = 0;
		loop {
			ensure!(i < args.len(), "unexpected end of (fn) form");

			match args[i] {
				Val::Sym(FLAG_NAME_SYM) => i += 2,
				Val::Sym(FLAG_ARG_LIMITS_SYM) => i += 3,
				Val::Arr(ref arr) => break (i, arr.clone()),
				ref arg => bail!("unexpected form {} in (fn)", arg)
			}
		}
	};

	let flags = &args[..params_i];
	let body = &args[params_i + 1..];

	//parse the params array as a collection of patterns
	if params_arr.len() == 0 {
		if atsign_params == false {
			macro_no_op!()
		} else {
			//convert %met-fn into fn
			return Ok(backquote!("(fn ~..flags () ~..body)"))
		}
	}

	let (params_pat, _) = pat_from_forms(
		&[Val::Arr(params_arr.clone())],
		atsign_params,
		params_arr.span()
	)?;

	let pats = match &params_pat.matcher {
		Matcher::Arr(pats) => pats.clone(),
		_ => bail_at!(params_arr.span(), "in (fn), {} is not a valid array pattern", params_arr)
	};

	//inspect each pattern to select a FnStrategy
	let mut strategy = FnStrategy::Trivial;

	fn matcher_is_trivial(matcher: &Matcher) -> bool {
		match matcher {
			Matcher::Underscore | Matcher::Sym(_) | Matcher::AtsignSym(_) => true,
			Matcher::Opt(sub_matcher, _) | Matcher::Rest(sub_matcher) => {
				match &**sub_matcher {
					Matcher::Underscore | Matcher::Sym(_) | Matcher::AtsignSym(_) => true,
					_ => false
				}
			}
			_ => false
		}
	}

	for (pat_i, pat) in pats.iter().enumerate() {

		//conditions for a pattern to be trivial: the matcher is trivial. if it's a rest, it's at
		//the end. if it's an opt with an initializer then its initializer is trivial.
		let opt_with_nontrivial_initializer = match &pat.matcher {
			Matcher::Opt(_, Some(val)) => {
				match val {
					Val::Nil | Val::Bool(_) | Val::Int(_) | Val::Flo(_) |
					Val::Char(_) | Val::Str(_) | Val::Tab(_) => false,
					Val::Arr(arr) if arr.len() == 0 => false,
					Val::Arr(arr) if arr.len() == 2 && 
					               arr.get::<Val>(0)? == Val::Sym(QUOTE_SYM) => false,
					_ => true
				}
			}
			_ => false
		};

		let nonterminal_rest = match &pat.matcher {
			Matcher::Rest(_) => pat_i != pats.len() - 1,
			_ => false
		};

		if pat.at.is_some() ||
		   pat.pred.is_some() ||
		   !matcher_is_trivial(&pat.matcher) || 
		   nonterminal_rest || 
		   opt_with_nontrivial_initializer {
		   	if strategy == FnStrategy::Trivial {
				strategy = FnStrategy::Simple;
			}
		} else {
			continue
		}

		//conditions for a non-trivial pattern to be simple rather than complex: opt initializers
		//are all trivial. rest param is at the end. all opt or rest params have trivial matchers.
		let nontrivial_opt_or_rest = match &pat.matcher {
			Matcher::Opt(sub_matcher, _) | Matcher::Rest(sub_matcher) => {
				pat.at.is_some() ||
		  		pat.pred.is_some() ||
				!matcher_is_trivial(sub_matcher)
			}
			_ => false
		};

		if nontrivial_opt_or_rest || 
		   nonterminal_rest ||
		   opt_with_nontrivial_initializer {
			strategy = FnStrategy::Complex;
			break
		}
	}

	//branch based on the selected FnStrategy
	match strategy {
		FnStrategy::Trivial => {
			//to avoid an endless loop, we need to macro_no_op when the input is already Trivial
			let mut to_no_op = true;
			for pat in &pats {
				assert!(pat.at.is_none() && pat.pred.is_none());

				match &pat.matcher {
					Matcher::Underscore | Matcher::AtsignSym(_) => {
						to_no_op = false;
						break
					}
					_ => ()
				}
			}

			if to_no_op && !atsign_params {
				macro_no_op!()
			}

			//Trivial strategy is almost a no-op, except that we need to replace any _ forms with 
			//a gensym, and any @name forms with name. the easiest way to achieve that is by 
			//serializing the Pats back into forms.
			fn trivial_matcher_to_form(matcher: &Matcher) -> Val {
				match matcher {
					Matcher::Sym(name) | Matcher::AtsignSym(name) => Val::Sym(*name),
					Matcher::Underscore => Val::Sym(glsp::gensym()),
					Matcher::Opt(sub_matcher, None) => {
						Val::Arr(arr![
							QUESTION_MARK_SYM,
							trivial_matcher_to_form(sub_matcher)
						])
					}
					Matcher::Opt(sub_matcher, Some(init_val)) => {
						Val::Arr(arr![
							QUESTION_MARK_SYM,
							trivial_matcher_to_form(sub_matcher),
							init_val.clone()
						])
					}
					Matcher::Rest(sub_matcher) => {
						Val::Arr(arr![
							SPLAY_SYM,
							trivial_matcher_to_form(sub_matcher)
						])
					}
					_ => panic!()
				}
			}

			let trivial_params = glsp::arr_from_iter(pats.iter().map(|pat| {
				trivial_matcher_to_form(&pat.matcher)
			}))?;

			Ok(backquote!("(fn ~..flags ~trivial_params ~..body)"))
		}
		FnStrategy::Simple => {
			//every parameter is replaced with a trivial gensym. we then codegen the equivalent of
			//(let pat gsN) for each gensym.
			let simple_params = glsp::arr();
			let pats_body = glsp::arr();

			let mut names = HashSet::new();
			for pat in &pats {
				pat.names(&mut names, false);
			}
			for name in names {
				let to_push: Val = backquote!("(let ~name #n)");
				pats_body.push(to_push)?;
			}

			for pat in &pats {
				let simple_name = glsp::gensym();

				let (simple_form, simple_pat) = match &pat.matcher {
					Matcher::Opt(sub_matcher, None) => {
						(Val::Arr(arr![QUESTION_MARK_SYM, simple_name]), 
						Some(Pat {
							matcher: (**sub_matcher).clone(),
							..pat.clone()
						}))
					}
					Matcher::Opt(sub_matcher, Some(init_val)) => {
						(Val::Arr(arr![QUESTION_MARK_SYM, simple_name, init_val.clone()]), 
						Some(Pat {
							matcher: (**sub_matcher).clone(),
							..pat.clone()
						}))
					}
					Matcher::Rest(sub_matcher) => {
						if let Matcher::Underscore = **sub_matcher {
							(Val::Arr(arr![SPLAY_SYM, UNDERSCORE_SYM]), None)
						} else {
							(Val::Arr(arr![SPLAY_SYM, simple_name]),
							Some(Pat {
								matcher: (**sub_matcher).clone(),
								..pat.clone()
							}))
						}
					}
					_ => (Val::Sym(simple_name), Some(pat.clone()))
				};
				simple_params.push(simple_form)?;

				if let Some(simple_pat) = simple_pat {
					simple_pat.codegen(
						simple_name,
						&pats_body,
						SetStrategy(PlaceStrategy::Local, AssignStrategy::Set),
						MismatchStrategy::Bail
					)?;
				}
			}

			Ok(backquote!("(fn ~..flags ~simple_params ~..pats_body ~..body)"))
		}
		FnStrategy::Complex => {
			//this is relatively straightforward
			let arg_name = glsp::gensym();
			let pat_body = glsp::arr();

			let mut names = HashSet::new();
			params_pat.names(&mut names, false);
			for name in names {
				let to_push: Val = backquote!("(let ~name #n)");
				pat_body.push(to_push)?;
			}

			params_pat.codegen(
				arg_name,
				&pat_body,
				SetStrategy(PlaceStrategy::Local, AssignStrategy::Set),
				MismatchStrategy::Bail
			)?;

			let (min_args, max_args) = params_pat.matcher.arg_limits();
			ensure!(!flags.contains(&Val::Sym(FLAG_ARG_LIMITS_SYM)), "unexpected &arg-limits flag");

			Ok(backquote!("
				(fn &arg-limits ~min_args ~max_args ~..flags (..~arg_name) ~..pat_body ~..body)
			"))
		}
	}
}

fn fn0(body: &[Val]) -> Root<Arr> {
	backquote!("(fn () ~..body)")
}

fn fn1(body: &[Val]) -> GResult<Root<Arr>> {
	fn replace_underscores(src: Val, replacement: Sym) -> GResult<Val> {
		// `src` is fully-expanded, so the only forms we need to skip are the first argument to
		// `let`, and anything within a nested `fn` or a `quote`.
		match src {
			Val::Arr(ref arr) if arr.len() > 0 => {
				let cloned = glsp::arr_from_iter(arr.iter())?;

				let head = cloned.get::<Val>(0)?;
				if head != Val::Sym(FN_SYM) && head != Val::Sym(QUOTE_SYM) {
					let start_i = if head == Val::Sym(LET_SYM) { 2 } else { 0 };
					for i in start_i .. cloned.len() {
						let child_form = cloned.get::<Val>(i)?;
						if child_form == Val::Sym(UNDERSCORE_SYM) {
							cloned.set(i, replacement)?;
						} else {
							cloned.set(i, replace_underscores(child_form, replacement)?)?;
						}
					}
				}

				Ok(Val::Arr(cloned))
			}
			_ => Ok(src)
		}
	}

	let mut expanded = glsp::expand_multi(body, Some(EnvMode::Copied))?;

	let param_name = glsp::gensym();
	for form in &mut expanded {
		*form = replace_underscores(form.clone(), param_name)?;
	}

	Ok(backquote!("(fn (~param_name) ~..expanded)"))
}

fn arrow_first(first: Val, rest: &[Val]) -> GResult<Val> {
	arrow(ArrowPos::First, first, rest)
}

fn arrow_last(first: Val, rest: &[Val]) -> GResult<Val> {
	arrow(ArrowPos::Last, first, rest)
}

enum ArrowPos {
	First,
	Last
}

fn arrow(pos: ArrowPos, first: Val, rest: &[Val]) -> GResult<Val> {
	if rest.len() == 0 {
		return Ok(first)
	}

	let mut result_name = glsp::gensym();
	let do_form: Root<Arr> = backquote!("(do (let ~result_name ~first))");

	for (i, val) in rest.iter().enumerate() {
		let (val, splayed) = match val {
			Val::Arr(arr) if arr.len() == 2 && arr.get::<Val>(0).unwrap() == Val::Sym(SPLAY_SYM) => {
				(arr.get(1).unwrap(), true)
			}
			val => (val.clone(), false)
		};

		let result_form: Val = if splayed {
			backquote!("(splay ~result_name)")
		} else {
			backquote!("~result_name")
		};

		let call_form: Root<Arr> = match val {
			Val::Sym(callee_name) => backquote!("(~callee_name ~result_form)"),
			Val::Arr(call_arr) if call_arr.len() >= 1 => {
				let callee: Val = call_arr.get(0)?;
				let args: Vec<Val> = Vec::from_iter(call_arr.iter().skip(1));

				let out_call_arr: Root<Arr> = match callee {
					Val::Sym(MET_NAME_SYM) | Val::Sym(QUESTION_MARK_SYM) | Val::Sym(ATSIGN_SYM) => {
						backquote!("(~call_arr ~result_form)")
					}
					callee => {
						match pos {
							ArrowPos::First => backquote!("(~callee ~result_form ~..args)"),
							ArrowPos::Last => backquote!("(~callee ~..args ~result_form)")
						}
					}
				};

				out_call_arr.set_span(call_arr.span());
				out_call_arr
			}
			val => bail!("expected a sym or a non-empty arr, received {}", val)
		};

		result_name = glsp::gensym();
		let do_clause = if i == rest.len() - 1 {
			call_form
		} else {
			backquote!("(let ~result_name ~call_form)")
		};
		do_form.push(do_clause)?;
	}

	Ok(Val::Arr(do_form))
}

fn question_mark(arg: Val) -> GResult<Val> {
	if let Val::Arr(arr) = arg {
		if arr.len() == 2 {
			if let Val::Sym(ATSIGN_SYM) = arr.get(0).unwrap() {
				let second = arr.get::<Val>(1).unwrap();
				return Ok(backquote!("(atsign-opt ~second)"))
			}
		}
	}

	macro_no_op!()
}

fn tab(clauses: &[Root<Arr>]) -> GResult<Root<Arr>> {
	if clauses.len() == 0 {
		macro_no_op!()
	}

	let len = clauses.len();
	let tab_name = glsp::gensym();
	let set_forms = glsp::arr_with_capacity(len);

	for clause in clauses.iter() {
		ensure!(clause.len() == 2, "expected (tab (k0 v0) (k1 v1) ..base)");

		let (key_form, val_form): (Val, Val) = (clause.get(0)?, clause.get(1)?);

		if key_form == Val::Sym(SPLAY_SYM) {
			let set_form: Root<Arr> = backquote!("(extend! ~tab_name ..~val_form)");
			set_forms.push(set_form)?;
		} else {
			let set_form: Root<Arr> = backquote!("(access= ~tab_name ~key_form ~val_form)");
			set_forms.push(set_form)?;
		}
	}

	Ok(backquote!(r#"
		(do
		  (let ~tab_name (tab))
		  ~..set_forms
		  ~tab_name)
	"#))
}

fn for_(args: &[Val]) -> GResult<Val> {
	let (_, forms_consumed) = pat_from_forms(args, false, Span::default())?;
	
	ensure!(args.len() >= forms_consumed + 2 && args[forms_consumed] == Val::Sym(IN_SYM),
	        "invalid for loop: expected (for pattern in iterable ...)");
	
	let pat_forms = &args[..forms_consumed];
	let iterable = args[forms_consumed + 1].clone();
	let body = &args[forms_consumed + 2..];

	Ok(backquote!(r#"
		(do
		  (let iter# (iter ~iterable))
		  (block LOOP
		    (let item# (iter-next! iter#))
		    (when (iter-finished? iter#)
		      (finish-block LOOP))
		    (let ~..pat_forms item#)
		    ~..body
		    (restart-block LOOP)))
	"#))
}

fn forn(clause: Root<Arr>, body: &[Val]) -> GResult<Val> {
	ensure!(clause.len() >= 2 && clause.len() <= 4 && clause.get::<Val>(0)?.is_sym(), 
	        "invalid forn clause {}", &clause);

	let rest = clause.shallow_clone();
	let name: Sym = rest.pop_start()?;

	Ok(backquote!(r#"
		(for ~name in (rn ~..rest)
		  ~..body)
	"#))
}

fn forni(clause: Root<Arr>, body: &[Val]) -> GResult<Val> {
	ensure!(clause.len() >= 2 && clause.len() <= 4 && clause.get::<Val>(0)?.is_sym(), 
	        "invalid forni clause {}", &clause);

	let rest = clause.shallow_clone();
	let name: Sym = rest.pop_start()?;

	Ok(backquote!(r#"
		(for ~name in (rni ~..rest)
		  ~..body)
	"#))
}

fn break_(result: Option<Val>) -> Val {
	match result {
		Some(result) => backquote!("(finish-block LOOP ~result)"),
		None => backquote!("(finish-block LOOP)")
	}
}

fn continue_() -> Val {
	backquote!("(restart-block LOOP)")
}

fn loop_(body: &[Val]) -> Val {
	backquote!(r#"
		(block LOOP
		  ~..body
		  (restart-block LOOP))
	"#)
}

fn yield_from(iterable: Val) -> Val {
	backquote!(r#"
		(do
		  (let iterable# ~iterable)
		  (cond
		    ((coro? iterable#)
		      (let result# (coro-run iterable#))
		      (while (eq? (coro-state iterable#) 'paused)
		      	(= result# (coro-run iterable# (yield result#))))
		      result#)
		    (else
		      (for item# in iterable#
		        (yield item#)))))
	"#)
}

fn defplace(accessor: Sym, setter: Sym, memoize_args: Option<bool>) -> Root<Arr> {
	match memoize_args {
		Some(memoize_args) => backquote!("(bind-place! '~accessor '~setter ~memoize_args)"),
		None => backquote!("(bind-place! '~accessor '~setter)")
	}
}

fn bind_place(
	std: &mut Std,
	accessor: Sym, 
	setter: Sym, 
	memoize_args: Option<bool>
) -> GResult<()> {

	match std.setters.entry(accessor) {
		Vacant(entry) => entry.insert((setter, memoize_args.unwrap_or(true))),
		Occupied(_) => bail!("duplicate setter {}", accessor)
	};

	Ok(())
}

fn set(std: &Std, args: &[Val]) -> GResult<Val> {
	ensure!(args.len() % 2 == 0, "= expects an even number of arguments");

	fn set_clause(std: &Std, place: Val, new_val_form: Val) -> GResult<Val> {
		ensure!(place.is_arr(), "invalid place {:?} passed to =", place);
		let place = place.unwrap_arr();

		ensure!(place.len() >= 1 && place.get::<Val>(0)?.is_sym(),
		        "invalid place {:?} passed to =", place);

		let callee: Sym = place.get(0)?;
		if callee == QUESTION_MARK_SYM {
			ensure!(place.len() == 2, "invalid place {:?} passed to =", place);
			let opt_place = place.get::<Val>(1)?;

			ensure!(opt_place.is_arr(), "invalid place {:?} passed to =", place);
			let opt_place = opt_place.unwrap_arr();

			ensure!(opt_place.len() >= 1 && opt_place.get::<Val>(0)?.is_sym(),
			        "invalid place {:?} passed to =", place);

			let opt_callee: Sym = opt_place.get(0)?;

			if let Some((opt_setter, _)) = std.opt_setters.get(&opt_callee) {
				let opt_args = glsp::arr_from_iter(opt_place.iter().skip(1))?;

				Ok(backquote!("(~opt_setter ~..opt_args ~new_val_form)"))
			} else {
				bail!("invalid place {:?} passed to =", place);
			}
		} else {
			if let Some((setter, _)) = std.setters.get(&callee) {
				let args = glsp::arr_from_iter(place.iter().skip(1))?;

				Ok(backquote!("(~setter ~..args ~new_val_form)"))
			} else {
				bail!("invalid place {:?} passed to =", place);
			}
		}
	}

	match args.len() {
		0 => Ok(Val::Nil),
		2 => {
			ensure!(!is_splay_form(&args[1]), "the second argument to = cannot be splayed");

			if args[0].is_sym() {
				macro_no_op!()
			}

			set_clause(std, args[0].clone(), args[1].clone())
		}
		_ => {
			let do_form = arr![DO_SYM];
			for pair in args.chunks_exact(2) {
				if pair[0].is_sym() {
					do_form.push(arr![ASSIGNMENT_SYM, ..pair])?;
				} else {
					do_form.push(set_clause(std, pair[0].clone(), pair[1].clone())?)?;
				}
			}

			Ok(Val::Arr(do_form))
		}
	}
}

fn in_place<F>(place: Val, f: F) -> GResult<Val> 
where
	F: FnOnce(Val) -> GResult<Val>
{
	match place {
		Val::Sym(sym) => {
			f(Val::Sym(sym))
		}

		Val::Arr(ref place) if place.len() >= 1 && place.get::<Val>(0)?.is_sym() => {
			let accessor = place.get::<Sym>(0)?;

			let std = Std::borrow();
			ensure!(std.setters.contains_key(&accessor), "unrecognized place form {:?}", place);

			let (_, memoize_args) = *std.setters.get(&accessor).unwrap();

			//under normal circumstances, we memoize each argument to the place into its own
			//local variable... for example, (inc! [ar (an-expensive-call)]) wouldn't evaluate
			//`ar` or (an-expensive-call) twice. however, we need to make an exception for macro
			//places like `atsign`, because the place's exact, un-memoized arguments might be 
			//semantically important.
			if memoize_args {
				let mut arg_lets = Vec::<Root<Arr>>::with_capacity(place.len() - 1);
				let mut arg_names = Vec::<Sym>::with_capacity(place.len() - 1);

				for arg_form in place.iter().skip(1) {
					let arg_name = glsp::gensym();

					arg_lets.push(backquote!("(let ~arg_name ~arg_form)"));
					arg_names.push(arg_name);
				}

				let op_form = f(backquote!("(~accessor ~..arg_names)"))?;

				Ok(backquote!(r#"
					(do
					  ~..arg_lets
					  ~op_form)
				"#))
			} else {
				f(Val::Arr(place.clone()))
			}
		}

		_ => bail!("unrecognized place form {:?}", place)
	}
}

fn inc_assign(place: Val, by: &[Val]) -> GResult<Val> {
	let by = if by.len() == 0 { &[Val::Int(1)] } else { by };
	in_place(place, |place| Ok(backquote!("(= ~place (+ ~place ~..by))")))
}

fn dec_assign(place: Val, by: &[Val]) -> GResult<Val> {
	let by = if by.len() == 0 { &[Val::Int(1)] } else { by };
	in_place(place, |place| Ok(backquote!("(= ~place (- ~place ~..by))")))
}

fn mul_assign(place: Val, by: &[Val]) -> GResult<Val> {
	ensure!(by.len() >= 1, "mul! expects a place and at least one additional argument");
	in_place(place, |place| Ok(backquote!("(= ~place (* ~place ~..by))")))
}

fn div_assign(place: Val, by: &[Val]) -> GResult<Val> {
	in_place(place, |place| Ok(backquote!("(= ~place (/ ~place ~..by))")))
}

fn div_euclid_assign(place: Val, by: &[Val]) -> GResult<Val> {
	in_place(place, |place| Ok(backquote!("(= ~place (div-euclid ~place ~..by))")))
}

fn rem_assign(place: Val, by: Val) -> GResult<Val> {
	in_place(place, |place| Ok(backquote!("(= ~place (% ~place ~by))")))
}

fn rem_euclid_assign(place: Val, by: Val) -> GResult<Val> {
	in_place(place, |place| Ok(backquote!("(= ~place (rem-euclid ~place ~by))")))
}

fn abs_assign(place: Val) -> GResult<Val> {
	in_place(place, |place| Ok(backquote!("(= ~place (abs ~place))")))
}

fn neg_assign(place: Val) -> GResult<Val> {
	in_place(place, |place| Ok(backquote!("(= ~place (- ~place))")))
}

fn seek_assign(place: Val, target: Val, step_by: Option<Val>) -> GResult<Val> {
	match step_by {
		Some(step_by) => {
			in_place(place, |place| Ok(backquote!("
				(= ~place (seek ~place ~target ~step_by))
			")))
		}
		None => {
			in_place(place, |place| Ok(backquote!("
				(= ~place (seek ~place ~target))
			")))
		}
	}
}

fn antiseek_assign(place: Val, anti_target: Val, step_by: Option<Val>) -> GResult<Val> {
	match step_by {
		Some(step_by) => {
			in_place(place, |place| Ok(backquote!("
				(= ~place (antiseek ~place ~anti_target ~step_by))
			")))
		}
		None => {
			in_place(place, |place| Ok(backquote!("
				(= ~place (antiseek ~place ~anti_target))
			")))
		}
	}
}

fn clamp_assign(place: Val, min: Val, max: Val) -> GResult<Val> {
	in_place(place, |place| Ok(backquote!("(= ~place (clamp ~place ~min ~max))")))
}

fn swap_assign(place0: Val, place1: Val) -> GResult<Val> {
	in_place(place0, |place0| {
		in_place(place1, |place1| {
			Ok(backquote!("
				(do
				  (let tmp-name# ~place0)
				  (= ~place0 ~place1)
				  (= ~place1 tmp-name#))
			"))
		})
	})
}
