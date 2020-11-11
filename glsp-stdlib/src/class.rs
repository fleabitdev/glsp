use glsp::{
	arr, Arr, bail, bail_at, Class, DequeAccess, DequeOps, 
	ensure, ensure_at, EnvMode, Expander, Expansion, FromVal, GFn,
	GResult, Obj, OrNil, rfn, Root, Span, Sym, stock_syms::*, Tab, Val
};
use glsp_proc_macros::{backquote};
use smallvec::{SmallVec};
use std::{str};
use std::collections::{HashMap, hash_map::Entry::{Occupied, Vacant}, HashSet, VecDeque};
use std::io::{Write};
use std::iter::{FromIterator};
use super::{Std};
use super::pat::{
	AssignStrategy, MismatchStrategy, Pat, pat_from_forms, PlaceStrategy, SetStrategy
};

pub fn init(_sandboxed: bool) -> GResult<()> {
	glsp::bind_rfn_macro("defclass", rfn!(defclass))?;
	glsp::bind_rfn_macro("let-class", rfn!(let_class))?;
	glsp::bind_rfn_macro("class", rfn!(class))?;

	glsp::bind_rfn_macro("defmixin", rfn!(defmixin))?;
	glsp::bind_rfn_macro("let-mixin", rfn!(let_mixin))?;
	glsp::bind_rfn_macro("mixin", rfn!(mixin))?;

	glsp::bind_rfn_macro("defclassmacro", rfn!(defclassmacro))?;
	glsp::bind_rfn("bind-classmacro!", rfn!(bind_classmacro))?;

	glsp::bind_rfn_macro("defstruct", rfn!(defstruct))?;
	glsp::bind_rfn("%struct-constructor-macro", rfn!(struct_constructor_macro))?;

	//todo: get rid of these
	glsp::bind_rfn("%eval-as-method", rfn!(eval_as_method))?;
	glsp::bind_rfn("%create-pseudo-method", rfn!(create_pseudo_method))?;

	glsp::bind_rfn("call-met", rfn!(call_meth))?;
	glsp::bind_rfn("call-met-opt", rfn!(call_met_opt))?;
	glsp::bind_rfn("has-met?", rfn!(has_metp))?;
	glsp::bind_rfn("call-base-raw", rfn!(call_base_raw))?;
	glsp::bind_rfn("is?", rfn!(isp))?;
	glsp::bind_rfn("class-name", rfn!(class_name))?;
	glsp::bind_rfn("class-of", rfn!(class_of))?;
	glsp::bind_rfn("class-has-mixin?", rfn!(class_has_mixinp))?;
	glsp::bind_rfn("class-mixins", rfn!(class_mixins))?;
	glsp::bind_rfn("mixin?", rfn!(mixinp))?;
	glsp::bind_rfn("enab!", rfn!(enab))?;
	glsp::bind_rfn("enab?", rfn!(enabp))?;
	glsp::bind_rfn("disab!", rfn!(disab))?;
	glsp::bind_rfn("has-state?", rfn!(has_statep))?;
	glsp::bind_rfn("obj-kill!", rfn!(obj_kill))?;
	glsp::bind_rfn("obj-killed?", rfn!(obj_killedp))?;
	glsp::bind_rfn("%make-class", rfn!(make_class))?;

	//todo: some way to query the arg-limits of a method
	
	Ok(())
}


//-------------------------------------------------------------------------------------------------
// class construction
//-------------------------------------------------------------------------------------------------

fn defclass(name: Sym, clauses: &[Val]) -> Val {
	backquote!(r#"
		(def ~name (class
		  (name ~name)
		  ~..clauses))
	"#)
}

fn let_class(name: Sym, clauses: &[Val]) -> Val {
	backquote!(r#"
		(let ~name (cond
		  ((has-global? 'storage#)
		    storage#)
		  (else
		    (bind-global! 'storage#)
		    (global= 'storage# (class
		      (name ~name)
		      ~..clauses))
		    storage#)))
	"#)
}

fn defmixin(name: Sym, clauses: &[Val]) -> Val {
	backquote!(r#"
		(def ~name (mixin
		  (name ~name)
		  ~..clauses))
	"#)
}

fn let_mixin(name: Sym, clauses: &[Val]) -> Val {
	backquote!(r#"
		(let ~name (cond
		  ((has-global? 'storage#)
		    storage#)
		  (else
		    (bind-global! 'storage#)
		    (global= 'storage# (mixin
		      (name ~name)
		      ~..clauses))
		    storage#)))
	"#)
}

//expand_class_clauses performs macro-expansion on all the *immediate* children of a (class ...), 
//(mixin ...), (state ...), (state* ...) or (fsm ...) form. it takes classmacros and `splice` 
//into account.

fn expand_class_clauses(std: &Std, clauses: &[Val]) -> GResult<VecDeque<Val>> {
	let mut unexpanded: VecDeque<Val> = clauses.iter().cloned().collect();
	let mut expanded: VecDeque<Val> = VecDeque::with_capacity(unexpanded.len());

	while let Some(to_expand) = unexpanded.pop_front() {
		//we perform repeated expansion in case, e.g., one classmacro expands into another
		match to_expand {
			Val::Arr(ref arr) if arr.len() > 0 && arr.get::<Val>(0)?.is_sym() => {
				let tag = arr.get::<Sym>(0)?;

				match tag {
					SPLICE_SYM => {
						for spliced in arr.iter().skip(1).rev() {
							unexpanded.push_front(spliced);
						}
					}
					tag if std.classmacros.contains_key(&tag) => {
						let expander = std.classmacros.get(&tag).unwrap().clone();

						match glsp::expand_1(&to_expand, Some(expander), Some(EnvMode::Copied))? {
							Expansion::ExpandedTo(val) => unexpanded.push_front(val),
							Expansion::MacroNoOp => expanded.push_back(to_expand),
							Expansion::NotAMacro => unreachable!()
						}
					}
					_ => expanded.push_back(to_expand)
				}
			}

			Val::Nil => (),

			_ => expanded.push_back(to_expand)
		}
	}

	Ok(expanded)
}

fn class(std: &Std, clauses: &[Val]) -> GResult<Val> {
	class_impl(std, clauses, false)
}

fn mixin(std: &Std, clauses: &[Val]) -> GResult<Val> {
	class_impl(std, clauses, true)
}

fn class_impl(std: &Std, clauses: &[Val], is_mixin: bool) -> GResult<Val> {
	let mut expanded = expand_class_clauses(std, clauses)?;

	//the result of this macro is a tab, producing a "raw class" which can be passed to the
	//(%make-class) rfn below. it's a fairly straight representation of the input syntax, except
	//that we perform syntax transformations on @forms within method bodies.
	let output: Root<Arr> = backquote!("(tab)");

	//if `name` or `mixins` are provided as leading forms, pop them and emit their initializers
	let mut class_name: Option<Sym> = None;
	let mut seen_mixins = false;

	while expanded.len() > 0 && (class_name.is_none() || !seen_mixins) {
		let clause = expanded.front().unwrap().clone();
		if let Val::Arr(ref arr) = clause {
			if arr.len() > 0 && arr.get::<Val>(0)?.is_sym() {
				let tag = arr.get::<Sym>(0)?;
				match tag {
					NAME_SYM => {
						expanded.pop_front().unwrap();

						ensure_at!(arr.span(), class_name.is_none(), "duplicate name clause");
						ensure_at!(arr.span(), arr.len() == 2 && arr.get::<Val>(1)?.is_sym(),
						           "invalid clause {}", arr);

						let name_sym = arr.get::<Sym>(1)?;
						class_name = Some(name_sym);

						check_name(name_sym, false)?;

						let to_push: Val = backquote!("('name '~name_sym)");
						output.push(to_push)?;
					}
					MIXIN_SYM => {
						expanded.pop_front().unwrap();

						ensure_at!(arr.span(), !seen_mixins, "duplicate mixins clause");
						seen_mixins = true;

						let mixins = SmallVec::<[Val; 16]>::from_iter(arr.iter().skip(1));

						let to_push: Val = backquote!("('mixin (arr ~..mixins))");
						output.push(to_push)?;
					}
					_ => break
				}
			} else {
				break
			}
		} else {
			break
		}
	}

	if !seen_mixins {
		let to_push: Val = backquote!("('mixin (arr))");
		output.push(to_push)?;
	}

	if is_mixin {
		ensure!(class_name.is_some(), "mixin is missing a (name) clause");
		ensure!(!seen_mixins, "the (mixin) clause is forbidden in mixins");
	}

	let mixinp_entry: Val = backquote!("('mixin? ~is_mixin)");
	output.push(mixinp_entry)?;

	//all other clauses are treated as children of the imaginary "Main" state. for each state,
	//we emit an empty (tab) form to this arr *at the moment we start processing its form* (to 
	//preserve the states' ordering), then gradually build up the form's bindings and children 
	//as we encounter toplevel forms which belong to that state.
	let states_form = backquote!("(arr)");
	let mut binding_forms = Vec::<Root<Arr>>::new();
	let mut init_forms = Vec::<Root<Arr>>::new();
	let mut fini_forms = Vec::<Root<Arr>>::new();

	process_class_state(
		std,
		is_mixin,
		&states_form,
		&mut binding_forms,
		&mut init_forms,
		&mut fini_forms,
		class_name,
		MAIN_SYM,
		if is_mixin { class_name.unwrap() } else { MAIN_SYM },
		true,
		&[],
		None,
		expanded
	)?;

	let states_entry: Val = backquote!("('states ~states_form)");
	output.push(states_entry)?;

	//we previously just emitted `('bindings (arr ~..binding_forms)), but this is greedy for
	//registers - with a hundred bindings, it requires a hundred scratch registers and two
	//hundred literals! instead, we iteratively build up the bindings array within a (do) form.
	let bindings_name = glsp::gensym();
	let bindings_do: Root<Arr> = backquote!("(do (let ~bindings_name (arr)))");
	for chunk in binding_forms.chunks(32) {
		let fn_form: Root<Arr> = backquote!("(fn ())");

		//we have to chunk the output into separate (fn) forms, so that we don't exceed
		//the 256-register limit
		for binding_form in chunk {
			let push_form: Val = backquote!("(push! ~bindings_name ~binding_form)");
			fn_form.push(push_form)?;
		}

		bindings_do.push(arr![fn_form])?;
	}
	bindings_do.push(bindings_name)?;

	let bindings_entry: Val = backquote!("('bindings ~bindings_do)");
	output.push(bindings_entry)?;

	let inits_entry: Val = backquote!("('inits (arr ~..init_forms))");
	output.push(inits_entry)?;
	let finis_entry: Val = backquote!("('finis (arr ~..fini_forms))");
	output.push(finis_entry)?;

	//finally, we "auto-gensym" the raw class, replacing each instance of name# with a gensym.
	//we don't need to take any special action to deal with nested backquote, because method
	//bodies will have already been expanded. this isn't quite the same as the (backquote) macro -
	//for example, we don't convert (a b c) into (arr 'a 'b 'c).
	fn auto_gensym(arg: Val, auto_gensyms: &mut HashMap<Sym, Sym>) -> GResult<Val> {
		match arg {
			Val::Arr(arr) if arr.len() == 0 => {
				Ok(Val::Arr(arr![]))
			}

			//unlikely that we'd encounter a nested backquote, but if so it's a no-op
			Val::Arr(arr) if arr.get::<Val>(0)? == Val::Sym(BACKQUOTE_SYM) => {
				Ok(Val::Arr(arr))
			}

			Val::Arr(arr) => {
				let result = glsp::arr_with_capacity(arr.len() + 1);

				//we need to "pass through" any Spans for transparent error-reporting
				result.set_span(arr.span());

				for item in arr.iter() {
					result.push(auto_gensym(item, auto_gensyms)?)?;
				}

				Ok(Val::Arr(result))
			}

			Val::Sym(sym) if sym.name().ends_with("#") => {
				let replaced_name = match auto_gensyms.entry(sym) {
					Occupied(entry) => *entry.get(),
					Vacant(entry) => {
						let name = sym.name();
						*entry.insert(glsp::gensym_with_tag(&name[..name.len() - 1])?)
					}
				};

				Ok(Val::Sym(replaced_name))
			}

			//we don't currently recurse for table literals. this is consistent with (backquote)
			_ => Ok(arg)
		}
	}

	let output = auto_gensym(Val::Arr(output), &mut HashMap::new())?;

	//finished!
	Ok(backquote!("(%make-class ~output)"))
}

//checks whether the name of a binding, state, class (etc.) is valid. currently checks for the : 
//character, and checks for a list of forbidden names like self, state-name and Main. a trailing 
//# is forbidden for class/mixin names and state names.
fn check_name(name: Sym, permits_hash: bool) -> GResult<()> {
	if name.name().contains(':') {
		bail!("the name '{}' contains the : character", name)
	}

	for reserved_sym in &[
		MAIN_SYM,
		SELF_SYM,
		CLASS_SYM,
		CLASS_NAME_SYM,
		STATE_NAME_SYM,
		FIELD_SYM,
		CONST_SYM,
		MET_SYM,
		WRAP_SYM,
		STATE_SYM,
		STATEX_SYM,
		BASE_SYM,
		ENAB_SYM,
		ENABP_SYM,
		DISAB_SYM,
		ATSIGN_SYM,
		ATSIGN_OPT_SYM,
		SET_ATSIGN_SYM,
		SET_ATSIGN_OPT_SYM
	] {
		ensure!(name != *reserved_sym, "the name '{}' is reserved", name);
	}

	if !permits_hash {
		ensure!(!name.name().ends_with('#'), "'{}' should not end with #", name);
	}

	Ok(())
}

fn process_class_state(
	std: &Std,
	is_mixin: bool,
	states_form: &Root<Arr>,
	binding_forms: &mut Vec<Root<Arr>>,
	init_forms: &mut Vec<Root<Arr>>,
	fini_forms: &mut Vec<Root<Arr>>,
	class_name: Option<Sym>,
	state_name: Sym,
	name_prefix: Sym,
	enabled_by_default: bool,
	fsm_sibling_names: &[Sym],
	parent: Option<Sym>,
	clauses: VecDeque<Val>
) -> GResult<()> {

	if state_name != MAIN_SYM {
		check_name(state_name, false)?;
	}

	let mut binding_names = HashMap::<Sym, usize>::new();
	let mut children_names = Vec::<Sym>::new();

	let make_qualified_name = |unqualified: Sym| -> GResult<Sym> {
		check_name(unqualified, true)?;

		let mut text_buffer = SmallVec::<[u8; 64]>::new();
		write!(&mut text_buffer, "{}:{}", name_prefix, unqualified).unwrap();
		glsp::sym(&str::from_utf8(&text_buffer[..]).unwrap())
	};

	//we go back and populate this form at the end
	let tab_form: Root<Tab> = glsp::tab();
	states_form.push(&tab_form)?;

	//for fields, we build up initializer forms as we go, and then prepend them to the start of
	//the (init) form where applicable. to achieve this, we need to delay processing of (init)
	//until all other forms have been processed, so that we don't miss any (field)s.
	let mut field_initializers = SmallVec::<[(Pat, Option<Val>); 8]>::new();

	//process each clause, in two passes. in the first pass, we check that the basic structure
	//of the clause is valid, and process any methods, constants and fields. on the second pass,
	//we process inits, finis, states and fsms. this separation ensures that fields are emitted
	//in the correct field-shadowing order, with fields in child states always shadowing their 
	//parent state, even if a (field) in the parent appears after the (state)/(fsm) form.
	for clause in clauses.iter().cloned() {
		ensure!(clause.is_arr(), "invalid clause {}", clause);

		let clause = clause.unwrap_arr();
		ensure_at!(clause.span(), clause.len() > 0 && clause.get::<Val>(0)?.is_sym(), 
		           "invalid clause {}", clause);

		let tag = clause.get::<Sym>(0)?;

		match tag {
			NAME_SYM => bail_at!(clause.span(),
			                     "`name` class forms must only appear at the beginning"),
			MIXIN_SYM => bail_at!(clause.span(),
			                     "`mixin` class forms must only appear at the beginning"),

			FIELD_SYM => {
				let args = SmallVec::<[Val; 8]>::from_iter(clause.iter().skip(1));
				ensure_at!(clause.span(), args.len() > 0, "empty field clause");

				let mut rest = &args[..];
				while rest.len() > 0 {
					let (pat, forms_consumed) = pat_from_forms(&rest[..], false, clause.span())?;

					let mut names_set = HashSet::new();
					pat.names(&mut names_set, false);
					let names = Vec::from_iter(names_set.into_iter());

					ensure_at!(clause.span(), names.len() > 0, "field clauses must introduce \
					           at least one new binding");

					for field_name in &names {
						let qualified_name = make_qualified_name(*field_name)?;

						ensure_at!(clause.span(), !binding_names.contains_key(field_name),
						           "the name {} is already bound", qualified_name);
						binding_names.insert(*field_name, binding_forms.len());

						binding_forms.push(backquote!(r#"
							(arr '~field_name '~qualified_name '~state_name 'field)
						"#));
					}

					rest = if rest.len() > forms_consumed {
						field_initializers.push((pat, Some(rest[forms_consumed].clone())));
						&rest[forms_consumed + 1..]
					} else {
						field_initializers.push((pat, None));
						&rest[forms_consumed..]
					};
				}
			}

			CONST_SYM => {
				let args = SmallVec::<[Val; 8]>::from_iter(clause.iter().skip(1));
				ensure_at!(clause.span(), args.len() > 0, "empty const clause");

				let mut rest = &args[..];
				while rest.len() > 0 {
					let (pat, forms_consumed) = pat_from_forms(&rest[..], false, clause.span())?;
					ensure_at!(clause.span(), rest.len() >= forms_consumed + 1,
					           "const clauses must always have an initializer");

					let init_form = rest[forms_consumed].clone();

					let mut names_set = HashSet::new();
					pat.names(&mut names_set, false);
					let names = Vec::from_iter(names_set.into_iter());

					ensure_at!(clause.span(), names.len() > 0, "const clauses must introduce \
					           at least one new binding");

					for (i, const_name) in names.iter().enumerate() {
						let qualified_name = make_qualified_name(*const_name)?;

						ensure_at!(clause.span(), !binding_names.contains_key(const_name),
						           "the name {} is already bound", qualified_name);
						binding_names.insert(*const_name, binding_forms.len());

						if i < names.len() - 1 {
							binding_forms.push(backquote!(r#"
								(arr '~const_name '~qualified_name '~state_name 'const)
							"#));
						} else {
							let initializer_fn = const_initializer_to_form(
								qualified_name,
								&names[..],
								&rest[..forms_consumed],
								init_form.clone()
							)?;

							binding_forms.push(backquote!(r#"
								(arr '~const_name '~qualified_name '~state_name 'const 
								     ~initializer_fn)
							"#));
						}
					}

					rest = &rest[forms_consumed + 1..];
				}
			}

			MET_SYM => {
				ensure_at!(clause.span(), clause.len() >= 3 && clause.get::<Val>(1)?.is_sym() &&
				           clause.get::<Val>(2)?.is_arr(), "invalid met clause");

				let met_name = clause.get::<Sym>(1)?;
				let qualified_name = make_qualified_name(met_name)?;

				ensure_at!(clause.span(), !binding_names.contains_key(&met_name),
				           "the name {} is already bound", qualified_name);
				binding_names.insert(met_name, binding_forms.len());

				let method_form = method_clause_to_form(
					class_name,
					state_name,
					name_prefix,
					Some(qualified_name),
					clause,
					None,
					None
				)?;

				binding_forms.push(backquote!("
					(arr '~met_name '~qualified_name '~state_name 'met ~method_form)
				"));
			}

			WRAP_SYM => {
				ensure_at!(clause.span(), clause.len() >= 3 && clause.get::<Val>(1)?.is_sym() &&
				        clause.get::<Val>(2)?.is_arr(), "invalid wrap clause");

				let target_name = clause.get::<Sym>(1)?;
				let (target_state, unqualified) = split_wrap_target(clause.span(), target_name)?;
				let qualified = make_qualified_name(unqualified)?;

				if target_state == UNDERSCORE_SYM {
					let method_form = method_clause_to_form(
						class_name,
						state_name,
						name_prefix,
						Some(qualified),
						clause,
						None,
						None
					)?;

					binding_forms.push(backquote!("
						(arr '~unqualified '~qualified '~state_name 'wildcard-wrap ~method_form)
					"));
				} else {
					ensure_at!(clause.span(), !binding_names.contains_key(&unqualified),
					           "the name {} is already bound", qualified);
					binding_names.insert(unqualified, binding_forms.len());

					let method_form = method_clause_to_form(
						class_name,
						state_name,
						name_prefix,
						Some(qualified),
						clause,
						None,
						None
					)?;
						
					binding_forms.push(backquote!("
						(arr '~unqualified '~qualified '~state_name 
						     'wrap '~target_name ~method_form)
					"));
				}
			}

			PROP_SYM => {
				//parse the (prop ...) clause
				let (
					prop_name, 
					initializer_form, 
					getter_form, 
					setter_form
				) = process_prop_clause(&clause)?;

				//bind the backing field
				let backing_name = if prop_name.name().ends_with('#') {
					let name = prop_name.name();
					glsp::sym(&format!("{}:field#", &name[..name.len() - 1]))?
				} else {
					glsp::sym(&format!("{}:field", prop_name))?
				};

				let qualified_backing_str = format!("{}:{}", name_prefix, backing_name);
				let qualified_backing_name = glsp::sym(&qualified_backing_str)?;

				ensure_at!(clause.span(), !binding_names.contains_key(&backing_name),
				           "the name {} is already bound", qualified_backing_name);

				let (pat, _) = pat_from_forms(
					&[Val::Sym(backing_name)],
					false,
					clause.span()
				)?;
				field_initializers.push((pat, initializer_form));

				binding_names.insert(backing_name, binding_forms.len());
				binding_forms.push(backquote!(r#"
					(arr '~backing_name '~qualified_backing_name '~state_name 'field)
				"#));

				//bind the property itself
				let qualified_name = make_qualified_name(prop_name)?;

				let (qualified_get, qualified_set) = if qualified_name.name().ends_with('#') {
					let name = qualified_name.name();
					let partial = &name[..name.len() - 1];
					(glsp::sym(&format!("{}:get#", partial))?, 
					 glsp::sym(&format!("{}:set#", partial))?)
				} else {
					(glsp::sym(&format!("{}:get", qualified_name))?, 
					 glsp::sym(&format!("{}:set", qualified_name))?)
				};

				ensure_at!(clause.span(), !binding_names.contains_key(&prop_name),
				           "the name {} is already bound", qualified_name);

				let getter_form = getter_form.map(|getter_form| {
					method_clause_to_form(
						class_name,
						state_name,
						name_prefix,
						Some(qualified_name),
						getter_form,
						Some(qualified_backing_name),
						None
					)
				}).transpose()?;

				let setter_form = setter_form.map(|setter_form| {
					method_clause_to_form(
						class_name,
						state_name,
						name_prefix,
						Some(qualified_name),
						setter_form,
						Some(qualified_backing_name),
						None
					)
				}).transpose()?;

				binding_names.insert(prop_name, binding_forms.len());
				binding_forms.push(backquote!("
					(arr '~prop_name '~qualified_name '~state_name 
					     'prop '~qualified_get '~qualified_set
					     ~getter_form ~setter_form)
				"));
			}

			WRAP_PROP_SYM => {
				//parse the (prop ...) clause
				let (
					prop_name, 
					initializer_form, 
					getter_form, 
					setter_form
				) = process_prop_clause(&clause)?;
				assert!(initializer_form.is_none());

				//emit the 'wrap-prop or 'wildcard-wrap-prop binding
				let (target_state, unqualified) = split_wrap_target(clause.span(), prop_name)?;
				let qualified = make_qualified_name(unqualified)?;

				let (qualified_get, qualified_set) = if qualified.name().ends_with('#') {
					let name = qualified.name();
					let partial = &name[..name.len() - 1];
					(glsp::sym(&format!("{}:get#", partial))?, 
					 glsp::sym(&format!("{}:set#", partial))?)
				} else {
					(glsp::sym(&format!("{}:get", qualified))?, 
					 glsp::sym(&format!("{}:set", qualified))?)
				};

				let getter_form = getter_form.map(|getter_form| {
					method_clause_to_form(class_name, state_name, name_prefix, 
					                      Some(qualified), getter_form, None, None)
				}).transpose()?;

				let setter_form = setter_form.map(|setter_form| {
					method_clause_to_form(class_name, state_name, name_prefix, 
					                      Some(qualified), setter_form, None, None)
				}).transpose()?;

				if target_state == UNDERSCORE_SYM {
					binding_forms.push(backquote!("
						(arr '~unqualified '~qualified '~state_name 
						     'wildcard-wrap-prop '~qualified_get '~qualified_set
						     ~getter_form ~setter_form)
					"));
				} else {
					ensure_at!(clause.span(), !binding_names.contains_key(&unqualified),
					           "the name {} is already bound", qualified);
					binding_names.insert(unqualified, binding_forms.len());

					binding_forms.push(backquote!("
						(arr '~unqualified '~qualified '~state_name 
						     'wrap-prop '~qualified_get '~qualified_set
						     '~prop_name ~getter_form ~setter_form)
					"));
				}
			}

			//postponed until the second pass
			INIT_SYM | INIT_STATE_SYM | INIT_MIXIN_SYM |
			FINI_SYM | FINI_STATE_SYM | FINI_MIXIN_SYM |
			FSM_SYM | STATE_SYM | STATEX_SYM => (),

			_ => bail_at!(clause.span(), "unrecognized class clause {}", tag)
		}
	}

	//second pass: init, fini and states
	let mut init_clause: Option<Root<Arr>> = None;
	let mut seen_fini = false;

	for clause in clauses.iter().cloned() {
		let clause = clause.unwrap_arr();
		let tag: Sym = clause.get(0).unwrap();

		match tag {
			INIT_SYM | INIT_MIXIN_SYM | INIT_STATE_SYM => {
				ensure_at!(clause.span(), clause.len() >= 2 && clause.get::<Val>(1)?.is_arr(),
				           "invalid ({}) form", tag);

				if state_name == MAIN_SYM {
					if is_mixin {
						ensure_at!(clause.span(), tag == INIT_MIXIN_SYM, "the toplevel of a \
						           mixin must use (init-mixin), not ({})", tag);
					} else {
						ensure_at!(clause.span(), tag == INIT_SYM, "the toplevel of a class must \
						           use (init), not ({})", tag);
					}
				} else {
					ensure_at!(clause.span(), tag == INIT_STATE_SYM, "states must use \
					           (init-state), not ({})", tag);
				}

				ensure_at!(clause.span(), init_clause.is_none(), "duplicate ({}) form", tag);
				init_clause = Some(clause);
			}

			FINI_SYM | FINI_MIXIN_SYM | FINI_STATE_SYM => {
				if state_name == MAIN_SYM {
					if is_mixin {
						ensure_at!(clause.span(), tag == FINI_MIXIN_SYM, "the toplevel of a \
						           mixin must use (fini-mixin), not ({})", tag);
					} else {
						ensure_at!(clause.span(), tag == FINI_SYM, "the toplevel of a class must \
						           use (fini), not ({})", tag);
					}
				} else {
					ensure_at!(clause.span(), tag == FINI_STATE_SYM, "states must use \
					           (fini-state), not ({})", tag);
				}

				ensure_at!(clause.span(), !seen_fini, "duplicate ({}) form", tag);

				let method_form = method_clause_to_form(
					class_name,
					state_name,
					name_prefix,
					None,
					clause,
					None,
					None
				)?;
					
				fini_forms.push(backquote!("(arr '~state_name ~method_form)"));
				seen_fini = true;
			}

			STATE_SYM | STATEX_SYM => {
				ensure_at!(clause.span(), clause.len() >= 2 && clause.get::<Val>(1)?.is_sym(),
				           "state is missing a name");

				let child_name = clause.get::<Sym>(1)?;
				children_names.push(child_name);

				let state_clauses = SmallVec::<[Val; 16]>::from_iter(clause.iter().skip(2));
				let state_expanded = expand_class_clauses(std, &state_clauses[..])?;

				process_class_state(
					std, 
					is_mixin,
					states_form,
					binding_forms,
					init_forms,
					fini_forms,
					class_name, 
					child_name,
					child_name, 
					tag == STATEX_SYM,
					&[], 
					Some(state_name), 
					state_expanded
				)?;
			}

			FSM_SYM => {
				//first pass: macro-expand any immediate children
				let fsm_clauses = SmallVec::<[Val; 8]>::from_iter(clause.iter().skip(1));
				let fsm_expanded = expand_class_clauses(std, &fsm_clauses[..])?;

				//second pass: collect fsm-sibling names and validate the immediate children
				let mut fsm_child_names = SmallVec::<[Sym; 8]>::new();
				let mut default_count = 0;

				for fsm_clause in &fsm_expanded {
					ensure_at!(clause.span(), fsm_clause.is_arr(), 
					           "invalid fsm clause {}", fsm_clause);
					let fsm_clause = fsm_clause.clone().unwrap_arr();

					ensure_at!(fsm_clause.span(), 
					           fsm_clause.len() >= 2 && fsm_clause.get::<Val>(0)?.is_sym(),
					           "invalid fsm clause {}", fsm_clause);

					let fsm_tag = fsm_clause.get::<Sym>(0)?;
					ensure_at!(fsm_clause.span(), fsm_tag == STATE_SYM || fsm_tag == STATEX_SYM,
					           "invalid fsm tag {}: the immediate children of an fsm form \
					            must be #n, (state) or (state*)", fsm_tag);

					if fsm_tag == STATEX_SYM {
						default_count += 1;
					}

					ensure_at!(fsm_clause.span(), fsm_clause.get::<Val>(1)?.is_sym(),
					           "{} clause missing a name", fsm_tag);
					let fsm_child_name = fsm_clause.get::<Sym>(1)?;

					fsm_child_names.push(fsm_child_name);
					children_names.push(fsm_child_name);
				}

				ensure_at!(clause.span(), default_count <= 1, "too many (state*) forms in \
				           (fsm) form");

				//third pass: recurse for each (state ...) or (state* ...) form
				for fsm_clause in &fsm_expanded {
					let fsm_clause = fsm_clause.clone().unwrap_arr();

					let fsm_tag = fsm_clause.get::<Sym>(0)?;
					let fsm_child_name = fsm_clause.get::<Sym>(1)?;

					let fsm_sibling_names = SmallVec::<[Sym; 8]>::from_iter(
						fsm_child_names.iter().copied().filter(|name| *name != fsm_child_name)
					);

					let fsm_child_clauses = SmallVec::<[Val; 16]>::from_iter(
						fsm_clause.iter().skip(2)
					);
					let fsm_child_expanded = expand_class_clauses(std, &fsm_child_clauses[..])?;

					process_class_state(
						std, 
						is_mixin,
						states_form,
						binding_forms,
						init_forms,
						fini_forms,
						class_name, 
						fsm_child_name,
						fsm_child_name, 
						fsm_tag == STATEX_SYM,
						&fsm_sibling_names[..], 
						Some(state_name), 
						fsm_child_expanded
					)?;
				}
			}

			//we already checked for invalid clauses during the first pass
			_ => ()
		}
	}

	//if there was no init clause, but at least one field-initializer, then we need to
	//synthesise an empty (init), (init-state) or (init-mixin)
	if init_clause.is_none() {
		if field_initializers.iter().any(|fi| fi.1.is_some()) {
			if state_name == MAIN_SYM {
				if is_mixin {
					init_clause = Some(backquote!("(init-mixin (..args) (@base ..args))"));
				} else {
					init_clause = Some(backquote!("(init ())"));
				}
			} else {
				init_clause = Some(backquote!("(init-state ())"));
			}
		}
	}

	//process the init clause
	if let Some(init_clause) = init_clause {
		let requires_next_index = init_clause.get::<Sym>(0)? == INIT_MIXIN_SYM;

		let init_method_form = method_clause_to_form(
			class_name,
			state_name,
			name_prefix,
			None,
			init_clause,
			None,
			Some(&field_initializers[..])
		)?;

		init_forms.push(backquote!("(arr '~state_name ~init_method_form ~requires_next_index)"));
	}

	//populate our placeholder state-definition (tab ...) form, and then we're finished
	if let Some(parent_name) = parent {
		tab_form.set(PARENT_SYM, parent_name)?;
	}

	tab_form.set(NAME_SYM, state_name)?;
	tab_form.set(ENABLED_BY_DEFAULTP_SYM, enabled_by_default)?;
	tab_form.set(FSM_SIBLINGS_SYM, fsm_sibling_names)?;
	tab_form.set(CHILDREN_SYM, children_names)?;

	tab_form.deep_freeze();

	Ok(())
}

//maps a function over an arr, but ensures that the newly-allocated arr has the same Span
fn span_map(arr: &Arr, mut f: impl FnMut(Val) -> GResult<Val>) -> GResult<Root<Arr>> {
	let result = glsp::arr_with_capacity(arr.len());
	for val in arr.iter() {
		result.push(f(val)?)?;
	}
	result.set_span(arr.span());
	Ok(result)
}

//if no other consts are accessed, we return a fn form with an arg count of 0. otherwise, we 
//return a fn form which requires one arg: a table of existing constants.
fn const_initializer_to_form(
	qualified_name: Sym,
	const_names: &[Sym],
	pat_forms: &[Val], 
	initializer: Val
) -> GResult<Val> {

	fn recursively_transform(
		tab_name: Sym,
		form: Val,
		const_tab_flag: &mut bool
	) -> GResult<Val> {

		match form {
			Val::Arr(arr) if arr.len() == 2 && arr.get::<Val>(0)? == Val::Sym(ATSIGN_SYM) => {
				ensure_at!(arr.span(), arr.get::<Val>(1)?.is_sym(), "only @sym-literal forms \
				           are supported in const initializers");

				*const_tab_flag = true;

				let const_name = arr.get::<Sym>(1)?;
				Ok(backquote!("[~tab_name '~const_name]"))
			}
			Val::Arr(arr) => {
				Ok(Val::Arr(span_map(&arr, |form| {
					recursively_transform(tab_name, form, const_tab_flag)
				})?))
			}
			_ => Ok(form)
		}
	}

	let tab_name = glsp::gensym();
	let mut const_tab_flag = false;

	let expanded_initializer = glsp::expand(&initializer, Some(EnvMode::Copied))?;
	let transformed_initializer = recursively_transform(
		tab_name, 
		expanded_initializer, 
		&mut const_tab_flag
	)?;

	let fn_name = glsp::sym(&format!("init-{}", qualified_name))?;

	let fn_form: Root<Arr> = if const_tab_flag {
		backquote!("(fn &name ~fn_name (~tab_name))")
	} else {
		backquote!("(fn &name ~fn_name ())")
	};

	let let_form: Val = backquote!("(let ~..pat_forms ~transformed_initializer)");
	fn_form.push(let_form)?;

	let arr_form: Val = backquote!("(arr ~..const_names)");
	fn_form.push(arr_form)?;

	Ok(Val::Arr(fn_form))
}

//todo: provide distinct apis for simple methods, initializers, and getters/setters
fn method_clause_to_form(
	class_name: Option<Sym>, 
	state_name: Sym,
	name_prefix: Sym,
	qualified_name: Option<Sym>,
	clause: Root<Arr>,
	prop_backing_name: Option<Sym>,
	field_initializers: Option<&[(Pat, Option<Val>)]>
) -> GResult<Root<Arr>> {

	/*
	this method takes a (met ...), (wrap ...), (init[-x] ...), etc. clause, and returns
	a (fn ...) expression for the method. this involves detecting whether the clause
	requires a next-index parameter, processing @params (including the special way that they're
	handled in (init) forms), and replacing (@base), @self, etc. etc. in the method body.
	*/

	//destructure the clause
	let tag: Sym = clause.get(0)?;
	let (params_i, body_start_i, is_wrap) = match tag {
		MET_SYM => (Some(2), 3, false),
		WRAP_SYM => (Some(2), 3, true),
		GET_SYM => (None, 1, prop_backing_name.is_none()),
		SET_SYM => (Some(1), 2, prop_backing_name.is_none()),
		INIT_SYM | INIT_STATE_SYM => (Some(1), 2, false),
		INIT_MIXIN_SYM => (Some(1), 2, true),
		FINI_SYM | FINI_STATE_SYM => (None, 1, false),
		FINI_MIXIN_SYM => (None, 1, false),
		_ => unreachable!()
	};

	let params_arr: Root<Arr> = match params_i {
		Some(i) => {
			let params: Val = clause.get(i)?;
			params.deep_clone()?.unwrap_arr()
		}
		None => arr![]
	};

	let body = glsp::arr_from_iter(clause.iter().skip(body_start_i))?;

	//generate a qualified name, to be passed in as the fn's &name flag
	let qualified_name = if let Some(qualified_name) = qualified_name {
		qualified_name
	} else {
		let qualified_str = format!("{}:{}", name_prefix, tag);
		glsp::sym(&qualified_str)?
	};

	//generate names for the "invisible" arguments
	let self_name = glsp::gensym();
	let base_index_name = if is_wrap { Some(glsp::gensym()) } else { None };

	//parse the param list to check it for @name bindings, storing them in the at_params HashSet.
	let (params_pat, _) = pat_from_forms(&[Val::Arr(params_arr.clone())], true, params_arr.span())?;
	let mut at_params = HashSet::new();
	params_pat.names(&mut at_params, true);

	//for field-initializers, we have two possibilities. either this is an (init) form, in which
	//case we want to prepend some pattern-matching code to initialize each field in its original
	//textual order. otherwise, it's a (met)/(wrap) form, so we just want to emit (= @x x) to
	//the start of the method body, for x in at_params, in an arbitrary order.
	let body_init = glsp::arr();

	if let Some(field_initializers) = field_initializers {

		//this is an (init) form. iterate through each field declaration in order. if any of its 
		//names are present in at_params, assert that it has no initializer, and emit (= @Y:x x)
		//for each such name. otherwise, emit pattern-matching code for its initializer, storing
		//the result of each pattern using (= @Y:x result).
		let mut init_at_params = at_params.clone();

		for &(ref pat, ref initializer) in field_initializers {
			let mut pat_names = HashSet::new();
			pat.names(&mut pat_names, false);

			for pat_name in &pat_names {
				let qualified_str = format!("{}:{}", name_prefix, pat_name);
				let qualified_sym = glsp::sym(&qualified_str).unwrap();

				if init_at_params.contains(&pat_name) {
					ensure_at!(clause.span(), initializer.is_none(), "the field {} cannot have \
					           both an initializer form and an @-param", qualified_sym);

					init_at_params.remove(&pat_name);

					let init_form: Val = backquote!("(= @~qualified_sym ~pat_name)");
					body_init.push(init_form)?;
				}
			}

			if let Some(initializer) = initializer {
				let init_name = glsp::gensym();
				let init_form: Val = backquote!("(let ~init_name ~initializer)");
				body_init.push(init_form)?;

				pat.codegen(
					init_name,
					&body_init,
					SetStrategy(PlaceStrategy::Atsign(name_prefix), AssignStrategy::Set),
					MismatchStrategy::Bail
				)?;
			}
		}

		//we are permissive if an (init) form provides an @-param naming something defined outside
		//the current state. under those circumstances, we just emit a generic (= @name name),
		//like any other method with an @-param. (todo: we should give this a more deterministic
		//execution order, in case @name is a property.)
		for at_param in init_at_params {
			let init_form: Val = backquote!("(= @~at_param ~at_param)");
			body_init.push(init_form)?;
		}

	} else {
		for at_param in at_params {
			let init_form: Val = backquote!("(= @~at_param ~at_param)");
			body_init.push(init_form)?;
		}
	}

	//macro-expand the method's (fn) form
	let fn_form: Val = backquote!("
		(%met-fn &name ~qualified_name ~params_arr ~..body_init ~..body)
	");
	let expanded_fn = glsp::expand(&fn_form, Some(EnvMode::Copied))?.unwrap_arr();
	let expanded_params_i: usize = expanded_fn.iter().position(|v| v.is_arr()).unwrap();
	let expanded_params_arr = expanded_fn.get::<Root<Arr>>(expanded_params_i)?;
	let expanded_body = glsp::arr_from_iter(expanded_fn.iter().skip(expanded_params_i + 1))?;

	//transform any remaining @-forms in the body
	fn recursively_transform(
		form: Val, 
		class_name: Option<Sym>,
		self_name: Sym, 
		base_index_name: Option<Sym>,
		prop_backing_name: Option<Sym>,
		state_name: Sym
	) -> GResult<Val> {

		let result = match form {
			//(= @form value), which the (=) macro expands to (atsign= form value)
			//(= (? @form) value), which the (=) macro expands to (atsign-opt= form value)
			Val::Arr(ref arr) if arr.len() == 3 && 
			                (arr.get::<Val>(0)? == Val::Sym(SET_ATSIGN_SYM) ||
			                 arr.get::<Val>(0)? == Val::Sym(SET_ATSIGN_OPT_SYM)) => {
				
				let is_opt = arr.get::<Val>(0)? == Val::Sym(SET_ATSIGN_OPT_SYM);			                 
				
				if let Val::Sym(var_name) = arr.get::<Val>(1)? {
					let third = recursively_transform(
						arr.get(2)?,
						class_name,
						self_name, 
						base_index_name,
						prop_backing_name,
						state_name
					)?;

					let callee = if is_opt { SET_ACCESS_OPT_SYM } else { SET_ACCESS_SYM };

					if var_name == FIELD_SYM {
						ensure_at!(arr.span(), prop_backing_name.is_some(),
						           "@field is only valid in a (prop ...) clause");
						let prop_backing_name = prop_backing_name.unwrap();

						Ok(backquote!("(~callee ~self_name '~prop_backing_name ~third)"))
					} else {
						Ok(backquote!("(~callee ~self_name '~var_name ~third)"))
					}
				} else {
					bail_at!(arr.span(), "invalid form {} in method", form)
				}
			}

			//@form
			//(? @form), which the (?) macro will have expanded to (atsign-opt form)
			Val::Arr(ref arr) if arr.len() == 2 && 
			                   (arr.get::<Val>(0)? == Val::Sym(ATSIGN_SYM) ||
			                   	arr.get::<Val>(0)? == Val::Sym(ATSIGN_OPT_SYM)) => {
			
				let is_opt = arr.get::<Val>(0)? == Val::Sym(ATSIGN_OPT_SYM);

				match arr.get::<Val>(1)? {
					Val::Sym(SELF_SYM) => {
						Ok(Val::Sym(self_name))
					}
					Val::Sym(CLASS_SYM) => {
						Ok(backquote!("(class-of ~self_name)"))
					}
					Val::Sym(CLASS_NAME_SYM) => {
						Ok(backquote!("(class-name (class-of ~self_name))"))
					}
					Val::Sym(STATE_NAME_SYM) => {
						Ok(backquote!("'~state_name"))
					}
					Val::Sym(FIELD_SYM) => {
						ensure_at!(arr.span(), prop_backing_name.is_some(),
						           "@field is only valid in a (prop ...) clause");
						let prop_backing_name = prop_backing_name.unwrap();
						if is_opt {
							Ok(backquote!("(access-opt ~self_name '~prop_backing_name)"))
						} else {
							Ok(backquote!("[~self_name '~prop_backing_name]"))
						}
					}
					Val::Sym(var_name) => {
						if is_opt {
							Ok(backquote!("(access-opt ~self_name '~var_name)"))
						} else {
							Ok(backquote!("[~self_name '~var_name]"))
						}
					}
					_ => bail_at!(arr.span(), "invalid form {} in method", form)
				}
			}

			//(@form ...)
			//((? @form) ...), which the (?) macro will have expanded to ((atsign-opt form) ...)
			Val::Arr(ref arr) if arr.len() >= 1 && arr.get::<Val>(0)?.is_arr() && {
					let first = arr.get::<Root<Arr>>(0)?;

					first.len() == 2 &&
					(first.get::<Val>(0)? == Val::Sym(ATSIGN_SYM) ||
					 first.get::<Val>(0)? == Val::Sym(ATSIGN_OPT_SYM))
				} => {

				let transformed_arg_forms = glsp::arr_with_capacity(arr.len() - 1);
				for arg_form in arr.iter().skip(1) {
					transformed_arg_forms.push(recursively_transform(
						arg_form,
						class_name,
						self_name,
						base_index_name,
						prop_backing_name,
						state_name
					)?)?;
				}

				let is_opt = arr.get::<Root<Arr>>(0)?.get::<Val>(0)? == Val::Sym(ATSIGN_OPT_SYM);
				let second = arr.get::<Root<Arr>>(0)?.get::<Val>(1)?;
				match second {
					Val::Sym(BASE_SYM) => {
						ensure_at!(arr.span(), base_index_name.is_some(), 
						           "@base in a non-wrap method");
						let bin = base_index_name.unwrap();
						Ok(backquote!("(call-base-raw ~self_name ~bin ~..transformed_arg_forms)"))
					}
					Val::Sym(ENAB_SYM) | Val::Sym(ENABP_SYM) | Val::Sym(DISAB_SYM) => {
						Ok(backquote!("(~second ~self_name ~..transformed_arg_forms)"))
					}
					Val::Sym(CLASS_SYM) => {
						Ok(backquote!("((class-of ~self_name) ~..transformed_arg_forms)"))
					}
					Val::Sym(FIELD_SYM) => {
						ensure_at!(arr.span(), prop_backing_name.is_some(),
						           "@field is only valid in a (prop ...) clause");
						let prop_backing_name = prop_backing_name.unwrap();
						if is_opt {
							Ok(backquote!("(call-met-opt '~prop_backing_name ~self_name
							                              ~..transformed_arg_forms)"))
						} else {
							Ok(backquote!("(call-met '~prop_backing_name ~self_name
							                          ~..transformed_arg_forms)"))
						}
					}
					Val::Sym(callee_name) => {
						if is_opt {
							Ok(backquote!("(call-met-opt '~callee_name ~self_name 
							                              ~..transformed_arg_forms)"))
						} else {
							Ok(backquote!("(call-met '~callee_name ~self_name 
							                          ~..transformed_arg_forms)"))

						}
					}
					_ => bail_at!(arr.span(), "invalid form {} in method", form)
				}
			}

			//'form
			Val::Arr(ref arr) if arr.len() >= 1 && arr.get::<Val>(0)? == Val::Sym(QUOTE_SYM) => {
				Ok(Val::Arr(arr.clone()))
			}

			//any other arr
			Val::Arr(ref arr) => {
				Ok(Val::Arr(span_map(&arr, |form| {
					recursively_transform(
						form,
						class_name,
						self_name,
						base_index_name,
						prop_backing_name,
						state_name
					)
				})?))
			}

			//any other value
			ref form => Ok(form.clone())
		}?;

		//if the input was an arr and the output is an arr, make sure they have the same Span
		match (&form, &result) {
			(&Val::Arr(ref form), &Val::Arr(ref result)) => result.set_span(form.span()),
			_ => ()
		}

		Ok(result)
	}

	let transformed_params_arr = expanded_params_arr.clone();

	/*let transformed_params_arr = span_map(&expanded_params_arr, |form| {
		recursively_transform(
			form,
			class_name,
			self_name,
			base_index_name,
			prop_backing_name,
			state_name
		)
	})?;*/

	let transformed_body = span_map(&expanded_body, |form| {
		recursively_transform(
			form,
			class_name,
			self_name,
			base_index_name,
			prop_backing_name,
			state_name
		)
	})?;

	//add the "invisible" arguments to the params list
	if let Some(base_index_name) = base_index_name {
		transformed_params_arr.push_start(base_index_name)?;
	}
	transformed_params_arr.push_start(self_name)?;

	//finished!
	Ok(backquote!("(fn &name ~qualified_name ~transformed_params_arr ~..transformed_body)"))
}

fn split_wrap_target(span: Span, target_name: Sym) -> GResult<(Sym, Sym)> {
	let name_str = target_name.name();
	ensure_at!(span, name_str.bytes().filter(|ch| *ch == b':').count() == 1,
	           "invalid wrap target {}", target_name);

	let name_str = target_name.name();
	let mut splitter = name_str.split(":");
	let state_name = splitter.next().unwrap();
	let unqualified_name = splitter.next().unwrap();
	assert!(splitter.next().is_none());

	ensure_at!(span, state_name.len() > 0 && unqualified_name.len() > 0,
	           "invalid wrap target {}", target_name);

	Ok((glsp::sym(state_name)?, glsp::sym(unqualified_name)?))
}

//takes a (prop ...) or (wrap-prop ...) clause, validates its syntax, and decomposes it 
//into (prop_name, initializer_form, getter_form, setter_form)
fn process_prop_clause(
	clause: &Root<Arr>
) -> GResult<(Sym, Option<Val>, Option<Root<Arr>>, Option<Root<Arr>>)> {

	let tag: Sym = clause.get(0)?;

	ensure_at!(clause.span(), clause.len() >= 3 && clause.len() <= 5 && 
	           clause.get::<Val>(1)?.is_sym(), "invalid ({}) clause", tag);

	let prop_name: Sym = clause.get(1)?;

	let mut initializer: Option<Val> = None;
	let mut getter: Option<Root<Arr>> = None;
	let mut setter: Option<Root<Arr>> = None;

	for i in 2 .. clause.len() {
		let item = clause.get::<Val>(i)?;
		if item.is_arr() {
			let item_arr = item.clone().unwrap_arr();
			if item_arr.len() >= 1 && item_arr.get::<Val>(0)?.is_sym() {
				let item_tag: Sym = item_arr.get(0)?;

				if item_tag == GET_SYM {
					ensure_at!(clause.span(), getter.is_none(), "duplicate (get ...) in ({} {})",
					           tag, prop_name);

					getter = if item_arr.len() == 1 {
						if tag == PROP_SYM {
							Some(backquote!("(get @field)"))
						} else {
							Some(backquote!("(get (@base))"))
						}
					} else {
						Some(item_arr.clone())
					};

					continue
				}

				if item_tag == SET_SYM {
					ensure_at!(clause.span(), setter.is_none(), "duplicate (set ...) in ({} {})",
					           tag, prop_name);

					setter = if item_arr.len() == 1 {
						if tag == PROP_SYM {
							Some(backquote!("(set (arg#) (= @field arg#))"))
						} else {
							Some(backquote!("(set (arg#) (@base arg#))"))
						}
					} else {
						Some(item_arr.clone())
					};

					continue
				}
			}
		}

		if i == 2 {
			ensure_at!(clause.span(), tag == PROP_SYM, "(wrap-prop) clauses may not include \
			           an initializer");
			initializer = Some(item);
		} else {
			bail_at!(clause.span(), "unexpected item {} in ({} {})", item, tag, prop_name)
		}
	}

	ensure_at!(clause.span(), getter.is_some() || setter.is_some(), "({} {}) should contain \
	           a (get ...), a (set ...), or both", tag, prop_name);

	Ok((prop_name, initializer, getter, setter))
}

fn eval_as_method(obj: Root<Obj>, to_eval: Val) -> GResult<Val> {
	let form = method_clause_to_form(
		obj.class().name(),
		MAIN_SYM,
		MAIN_SYM,
		None,
		backquote!("(met m () ~to_eval)"),
		None,
		None
	)?;
	let to_call = glsp::eval(&Val::Arr(form), Some(EnvMode::Copied))?.unwrap_gfn();
	glsp::call(&to_call, &[obj])
}

fn create_pseudo_method(
	OrNil(class_name): OrNil<Sym>, 
	method_name: Sym,
	args: Root<Arr>, 
	body: Root<Arr>
) -> GResult<Val> {

	let form = method_clause_to_form(
		class_name,
		MAIN_SYM,
		MAIN_SYM,
		Some(method_name),
		backquote!("(met ~method_name ~args ~..body)"),
		None,
		None
	)?;
	glsp::eval(&Val::Arr(form), Some(EnvMode::Copied))
}

fn defclassmacro(name: Sym, fn_forms: &[Val]) -> Val {
	backquote!("(bind-classmacro! '~name (fn &name ~name ~..fn_forms))")
}

fn bind_classmacro(std: &mut Std, name: Sym, gfn: Root<GFn>) -> GResult<()> {
	ensure!(!std.classmacros.contains_key(&name), "duplicate classmacro {}", name);
	std.classmacros.insert(name, Expander::GFn(gfn));

	Ok(())
}

fn defstruct(name: Sym, clauses: &[Val]) -> GResult<Val> {
	//input syntax: any number of bare syms, followed by any number of (met) clauses, (prop) 
	//clauses, and (const) clauses. we emit a (field) for each sym; an (init); and implementations
	//for (met op-eq? ...) and (met op-clone ...) if they're not already present. we also
	//bind Name:new to the class, and bind a constructor macro to the Name.
	let mut clause_stack = Vec::from_iter(clauses.iter().rev().cloned());

	let mut field_names = Vec::<Sym>::new();
	let mut field_clauses = Vec::<Val>::new();
	let mut init_params = Vec::<Val>::new();

	//collect the field names and generate an (init) form
	while let Some(&Val::Sym(_)) = clause_stack.last() {
		let field_name = Sym::from_val(&clause_stack.pop().unwrap())?;
		field_names.push(field_name);
		field_clauses.push(backquote!("(field ~field_name)"));
		init_params.push(backquote!("@~field_name"));
	}

	//validate the remaining clauses, and check for (met op-eq?) and (met op-clone)
	let mut seen_eq = false;
	let mut seen_clone = false;
	for clause in clause_stack.iter().rev() {
		match clause {
			Val::Arr(arr) => {
				if arr.len() >= 1 && arr.get::<Val>(0)?.is_sym() {
					let tag: Sym = arr.get(0)?;

					match tag {
						PROP_SYM | CONST_SYM => (),
						MET_SYM => {
							if arr.len() >= 2 {
								match arr.get::<Val>(1)? {
									Val::Sym(OP_EQP_SYM) => seen_eq = true,
									Val::Sym(OP_CLONE_SYM) => seen_clone = true,
									_ => ()
								}
							}
						}
						FIELD_SYM => bail!("in defstruct, write x rather than (field x)"),
						_ => bail!("invalid defstruct clause ({} ...)", tag)
					}
				} else {
					bail!("invalid struct clause {}", arr)
				}
			}
			Val::Sym(_) => bail!("invalid struct clause: all field names must be grouped together \
			                    at the beginning of the struct definition"),
			val => bail!("invalid struct clause: expected an arr, receved {}", val.a_type_name())
		}
	}

	//generate the Name::new and Name? syms
	let name_new = glsp::sym(&format!("{}:new", name))?;
	let namep = glsp::sym(&format!("{}?", name))?;

	//emit op-eq? and op-clone methods
	if !seen_eq {
		let other_name = glsp::gensym_with_tag("other")?;

		let mut eq_forms = Vec::<Val>::new();
		for &field_name in &field_names {
			eq_forms.push(backquote!("(eq? @~field_name [~other_name '~field_name])"));
		}

		clause_stack.push(backquote!(r#"
			(met op-eq? (~other_name)
			  (and ~..eq_forms))
		"#));
	}

	if !seen_clone {
		clause_stack.push(backquote!(r#"
			(met op-clone ()
			  (~name_new ~..init_params))
		"#));
	}

	//bring it all together
	clause_stack.reverse();

	Ok(backquote!(r#"
		(do
		  (def ~name (class
		    (name ~name)
		    ~..field_clauses
		    (init (~..init_params))
		    ~..clause_stack))
  
  		  (def ~name_new ~name)

  		  (defn ~namep (any)
  		    (is? any ~name))
  
  		  (defmacro ~name (..args)
		    (%struct-constructor-macro '~name '~name_new '(~..field_names) ..args)))
	"#))
}

fn struct_constructor_macro(
	name: Sym,
	name_new: Sym,
	field_names: Vec<Sym>,
	args: &[Val]
) -> GResult<Val> {

	let mut remaining_names: HashMap<Sym, usize> = HashMap::from_iter(field_names.iter()
		.enumerate()
		.map(|(i, name)| (*name, i))
	);
	let mut local_bindings = Vec::<Val>::with_capacity(field_names.len());
	let mut constructor_args = vec![Val::Nil; field_names.len()];

	//we require the arguments to be an exhaustive exact match, but not necessarily in the
	//correct order. acceptable arguments are field-name, @field-name and (field-name form).
	//we guarantee that order-of-evaluation matches the input by binding each input to a
	//gensymmed local. there can be a single trailing ..base-form, in which case we bind
	//it to a gensymmed local and look up each remaining field dynamically.
	for (i, arg) in args.iter().enumerate() {
		let (field_name, init_form) = match *arg {
			Val::Sym(field_name) => {
				(field_name, Val::Sym(field_name))
			}
			Val::Arr(ref arr) if arr.len() == 2 && arr.get::<Val>(0)?.is_sym() => {
				match arr.get::<Sym>(0)? {
					SPLAY_SYM => {
						ensure_at!(arr.span(), i == args.len() - 1,
						           "..base struct constructor argument must appear last");

						let base_gensym = glsp::gensym();
						let base_form: Val = arr.get(1)?;
						local_bindings.push(backquote!("(let ~base_gensym ~base_form)"));

						for (field_name, arg_i) in remaining_names.drain() {
							let arg_gensym = glsp::gensym();
							local_bindings.push(backquote!(r#"
								(let ~arg_gensym [~base_gensym '~field_name])
							"#));
							constructor_args[arg_i] = Val::Sym(arg_gensym);
						}

						break
					}
					ATSIGN_SYM => {
						ensure_at!(arr.span(), arr.get::<Val>(1)?.is_sym(), "invalid @ form");
						let field_name: Sym = arr.get(1)?;
						(field_name, Val::Arr(arr.clone()))
					}
					field_name => {
						(field_name, arr.get(1)?)
					}
				}
			}
			ref val => bail!("invalid {} struct constructor argument {}", name, val)
		};

		match remaining_names.remove(&field_name) {
			Some(arg_i) => {
				let gensym = glsp::gensym();
				local_bindings.push(backquote!("(let ~gensym ~init_form)"));
				constructor_args[arg_i] = Val::Sym(gensym);
			}
			None => {
				if !field_names.contains(&field_name) {
					bail!("{} is not a member of the {} struct", field_name, name)
				} else {
					bail!("duplicate {} in {} constructor", field_name, name)
				}
			} 
		}
	}

	//check that this constructor was exhauastive
	match remaining_names.len() {
		0 => (),
		1 => bail!("{} struct constructor is missing the field {}", 
		           name, remaining_names.drain().next().unwrap().0),
		n => bail!("{} struct constructor is missing {} fields", name, n)
	}

	//emit the actual output forms...
	Ok(backquote!(r#"
		(do
		  ~..local_bindings
		  (~name_new ~..constructor_args))
	"#))
}


//-------------------------------------------------------------------------------------------------
// class-related rfns
//-------------------------------------------------------------------------------------------------

fn isp(arg: Val, class: Val) -> bool {
	match (arg, class) {
		(Val::Obj(obj), Val::Class(class)) => obj.is(&class),
		(Val::RData(rdata), Val::Sym(sym)) => rdata.class_name() == sym,
		_ => false
	}
}

fn class_has_mixinp(class: &Class, mixin: Root<Class>) -> bool {
	class.has_mixin(&mixin)
}

fn class_mixins(class: &Class) -> Root<Arr> {
	class.mixins()
}

fn mixinp(class: &Class) -> bool {
	class.is_mixin()
}

fn class_of(arg: Val) -> GResult<Val> {
	match arg {
		Val::Obj(obj) => Ok(Val::Class(obj.class())),
		Val::RData(rdata) => Ok(Val::Sym(rdata.class_name())),
		val => bail!("expected an obj or an rdata, received {}", val.a_type_name())
	}
}

fn call_meth(method_name: Sym, rcv: Val, args: &[Val]) -> GResult<Val> {
	match rcv {
		Val::Obj(obj) => obj.call(method_name, args),
		Val::Class(class) => class.call(method_name, args),
		Val::RData(rdata) => rdata.call(method_name, args),
		val => bail!("expected an obj, class or rdata, received {}", val.a_type_name())
	}
}

fn call_met_opt(method_name: Sym, rcv: Val, args: &[Val]) -> GResult<Option<Val>> {
	match rcv {
		Val::Obj(obj) => obj.call_if_present(method_name, args),
		Val::Class(class) => class.call_if_present(method_name, args),
		Val::RData(rdata) => rdata.call_if_present(method_name, args),
		val => bail!("expected an obj, class or rdata, received {}", val.a_type_name())
	}
}

fn has_metp(rcv: Val, method_name: Sym) -> GResult<bool> {
	match rcv {
		Val::Obj(obj) => obj.has_met(method_name),
		Val::Class(class) => class.has_met(method_name),
		Val::RData(rdata) => rdata.has_met(method_name),
		val => bail!("expected an obj, class or rdata, received {}", val.a_type_name())
	}
}

fn call_base_raw(obj: &Obj, base: Val, args: &[Val]) -> GResult<Val> {
	match base {
		Val::Int(index) if index >= 0 => obj.raw_call(index as usize, args),
		Val::Nil => Ok(Val::Nil),
		_ => panic!()
	}
}

fn has_statep(val: Val, state_name: Sym) -> GResult<bool> {
	match val {
		Val::Obj(obj) => Ok(obj.has_state(state_name).unwrap()),
		Val::Class(class) => Ok(class.has_state(state_name).unwrap()),
		val => bail!("expected an obj or class, received {}", val.a_type_name())
	}
}

fn enab(obj: &Obj, state_name: Sym, args: &[Val]) -> GResult<()> {
	obj.enab(state_name, args)
}

fn enabp(obj: &Obj, state_name: Sym) -> GResult<bool> {
	obj.is_enab(state_name)
}

fn disab(obj: &Obj, state_name: Sym) -> GResult<()> {
	obj.disab(state_name)
}

fn obj_kill(obj: &Obj) -> GResult<()> {
	obj.kill()
}

fn obj_killedp(obj: &Obj) -> bool {
	obj.is_killed()
}

fn class_name(class: &Class) -> Option<Sym> {
	class.name()
}

fn make_class(raw_class: &Tab) -> GResult<Root<Class>> {
	glsp::class(raw_class)
}
