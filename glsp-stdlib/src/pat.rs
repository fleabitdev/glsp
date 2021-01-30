use glsp::{
    bail, bail_at, ensure, ensure_at, stock_syms::*, Arr, DequeAccess, DequeOps, FromVal, GResult,
    Root, Span, Sym, Val,
};
use glsp_proc_macros::backquote;
use smallvec::SmallVec;
use std::collections::HashSet;
use std::fmt;

//-------------------------------------------------------------------------------------------------
// ast for patterns
//-------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub(crate) struct Pat {
    pub(crate) span: Span,
    pub(crate) at: Option<Sym>,
    pub(crate) matcher: Matcher,
    pub(crate) pred: Option<Pred>,
}

#[derive(Clone)]
pub(crate) enum Matcher {
    Underscore,
    Sym(Sym),
    AtsignSym(Sym),
    Literal(Val),
    Or(Vec<Pat>),

    Arr(Vec<Pat>),
    Index(Vec<(Val, Pat)>), //(key, pattern)
    Opt(Box<Matcher>, Option<Val>),
    Rest(Box<Matcher>),
}

#[derive(Clone)]
pub(crate) enum Pred {
    Sym(Sym),
    Form(Val),
    Not(Box<Pred>),
    Or(Vec<Pred>),
    And(Vec<Pred>),
}

impl fmt::Display for Pred {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pred::Sym(sym) => write!(f, "{}", sym),
            Pred::Form(val) => write!(f, "{}", val),
            Pred::Not(pred) => write!(f, "(not {})", pred),
            Pred::Or(preds) | Pred::And(preds) => {
                write!(
                    f,
                    "{}",
                    if matches!(self, Pred::Or(_)) {
                        "(or"
                    } else {
                        "(and"
                    }
                )?;
                for pred in preds {
                    write!(f, " {}", pred)?;
                }
                write!(f, ")")
            }
        }
    }
}

//returns a pattern, and the number of Vals consumed from the input slice. when atsign_params is
//true, @name will produce an AtsignSym matcher; otherwise, @sym is an error.
pub(crate) fn pat_from_forms(
    forms: &[Val],
    atsign_params: bool,
    span: Span,
) -> GResult<(Pat, usize)> {
    //extract forms/syms from the input
    let (at_name, matcher_form, pred_form, forms_consumed) = match forms {
        [] => bail_at!(span, "expected a pattern"),
        [Val::Sym(AT_SYM), ..] => bail_at!(span, "patterns may not begin with at"),
        [Val::Sym(COLON_SYM), ..] => bail_at!(span, "patterns may not begin with :"),
        [Val::Sym(at_name), Val::Sym(AT_SYM), matcher_form, Val::Sym(COLON_SYM), pred_form, ..] => {
            (
                Some(*at_name),
                matcher_form.clone(),
                Some(pred_form.clone()),
                5,
            )
        }
        [Val::Sym(at_name), Val::Sym(AT_SYM), matcher_form, ..] => {
            (Some(*at_name), matcher_form.clone(), None, 3)
        }
        [matcher_form, Val::Sym(COLON_SYM), pred_form, ..] => {
            (None, matcher_form.clone(), Some(pred_form.clone()), 3)
        }
        [matcher_form, ..] => (None, matcher_form.clone(), None, 1),
    };

    //parse the matcher and the predicate
    let matcher = matcher_from_form(matcher_form, atsign_params, span)?;
    let pred = pred_form
        .map(|pred_form| pred_from_form(pred_form, span))
        .transpose()?;

    //finished!
    let pat = Pat {
        span,
        at: at_name,
        matcher,
        pred,
    };

    Ok((pat, forms_consumed))
}

//unlike patterns, matchers and preds are always represented by a single Val
fn matcher_from_form(form: Val, atsign_params: bool, span: Span) -> GResult<Matcher> {
    let literal_form: Val = match form {
        Val::Sym(UNDERSCORE_SYM) => return Ok(Matcher::Underscore),
        Val::Sym(name) => {
            ensure!(
                !name.name().ends_with(':'),
                "in patterns, : is whitespace-sensitive"
            );
            return Ok(Matcher::Sym(name));
        }
        Val::Arr(arr) if arr.len() > 0 => {
            match arr.get(0)? {
                Val::Sym(ATSIGN_SYM) => {
                    assert!(atsign_params);
                    ensure_at!(
                        arr.span(),
                        atsign_params,
                        "@ matcher forbidden in this pattern"
                    );
                    ensure_at!(arr.span(), arr.len() == 2, "invalid @ form in pattern");

                    let second: Val = arr.get(1)?;
                    ensure_at!(arr.span(), second.is_sym(), "invalid @ form in pattern");

                    return Ok(Matcher::AtsignSym(Sym::from_val(&second)?));
                }
                Val::Sym(OR_SYM) => {
                    let elements: SmallVec<[Val; 8]> = arr.iter().skip(1).collect();
                    let mut remaining = &elements[..];

                    let mut pats = Vec::<Pat>::new();
                    while !remaining.is_empty() {
                        let (pat, forms_consumed) =
                            pat_from_forms(remaining, atsign_params, arr.span())?;

                        pats.push(pat);
                        remaining = &remaining[forms_consumed..];
                    }

                    return Ok(Matcher::Or(pats));
                }
                Val::Sym(QUESTION_MARK_SYM) => {
                    ensure_at!(
                        arr.span(),
                        arr.len() == 2 || arr.len() == 3,
                        "invalid ? form in pattern"
                    );
                    let matcher = matcher_from_form(arr.get(1)?, atsign_params, arr.span())?;
                    let default = if arr.len() == 3 {
                        Some(arr.get(2)?)
                    } else {
                        None
                    };

                    return Ok(Matcher::Opt(Box::new(matcher), default));
                }
                Val::Sym(SPLAY_SYM) => {
                    ensure_at!(arr.span(), arr.len() == 2, "invalid .. form in pattern");
                    let matcher = matcher_from_form(arr.get(1)?, atsign_params, arr.span())?;

                    return Ok(Matcher::Rest(Box::new(matcher)));
                }
                Val::Sym(ACCESS_SYM) => {
                    //each child of the [] form happens to also be a valid pattern. parse the list
                    //of patterns and then perform post-processing/validation on it. we temporarily
                    //set each key to Val::Nil at this stage.
                    let elements: SmallVec<[Val; 8]> = arr.iter().skip(1).collect();
                    let mut remaining = &elements[..];

                    let mut pairs = Vec::<(Val, Pat)>::new();
                    while !remaining.is_empty() {
                        let (pat, forms_consumed) =
                            pat_from_forms(remaining, atsign_params, arr.span())?;

                        pairs.push((Val::Nil, pat));
                        remaining = &remaining[forms_consumed..];
                    }

                    //the only valid matchers are Matcher::[Atsign]Sym, Matcher::Opt,
                    //Matcher::Rest,or a Matcher::Arr of length two where the first sub-pattern
                    //is a Matcher::[Atsign]Sym or Matcher::Literal, and the second is not
                    //a Matcher::Rest
                    for (key, pat) in &mut pairs {
                        match &pat.matcher {
                            Matcher::Sym(name) | Matcher::AtsignSym(name) => {
                                *key = Val::Sym(*name);
                            }
                            Matcher::Opt(opt_matcher, _) => match &**opt_matcher {
                                Matcher::Sym(name) | Matcher::AtsignSym(name) => {
                                    *key = Val::Sym(*name);
                                }
                                _ => bail_at!(arr.span(), "invalid ? sub-pattern in []"),
                            },
                            Matcher::Rest(_) => (),
                            Matcher::Arr(pats) if pats.len() == 2 => {
                                ensure_at!(
                                    arr.span(),
                                    pats[0].at.is_none() && pats[0].pred.is_none(),
                                    "invalid key in [(key pat)]"
                                );

                                match &pats[0].matcher {
                                    Matcher::Sym(name) | Matcher::AtsignSym(name) => {
                                        *key = Val::Sym(*name);
                                    }
                                    Matcher::Literal(val) => {
                                        *key = val.clone();
                                    }
                                    _ => bail_at!(arr.span(), "invalid key in [(key pat)]"),
                                }

                                ensure!(
                                    !matches!(&pats[1].matcher, Matcher::Rest(_)),
                                    "[(key ..pat)] is not a valid pattern"
                                );

                                let second = pats[1].clone();
                                *pat = second;
                            }
                            _ => bail_at!(
                                arr.span(),
                                "invalid sub-pattern in []: should be a \
                                          symbol, ?, .., or an array of length two"
                            ),
                        }
                    }

                    return Ok(Matcher::Index(pairs));
                }
                Val::Sym(QUOTE_SYM) => {
                    ensure_at!(arr.span(), arr.len() == 2, "invalid ' form in pattern");
                    arr.get(1)?
                }
                _ => {
                    let elements: SmallVec<[Val; 8]> = arr.iter().collect();
                    let mut remaining = &elements[..];

                    let mut pats = Vec::<Pat>::new();
                    while !remaining.is_empty() {
                        let (pat, forms_consumed) =
                            pat_from_forms(remaining, atsign_params, arr.span())?;

                        pats.push(pat);
                        remaining = &remaining[forms_consumed..];
                    }

                    return Ok(Matcher::Arr(pats));
                }
            }
        }
        literal => literal,
    };

    match literal_form {
        Val::Nil
        | Val::Bool(_)
        | Val::Int(_)
        | Val::Flo(_)
        | Val::Char(_)
        | Val::Sym(_)
        | Val::Arr(_)
        | Val::Tab(_)
        | Val::Str(_) => Ok(Matcher::Literal(literal_form)),
        Val::GIter(..)
        | Val::Obj(..)
        | Val::Class(..)
        | Val::GFn(..)
        | Val::RFn(..)
        | Val::Coro(..)
        | Val::RData(..) => {
            bail_at!(
                span,
                "non-syntax value {} encountered in pattern",
                literal_form
            )
        }
    }
}

fn pred_from_form(form: Val, span: Span) -> GResult<Pred> {
    match form {
        Val::Sym(name) => Ok(Pred::Sym(name)),
        Val::Arr(ref arr) if arr.len() > 0 => match arr.get(0)? {
            Val::Sym(NOT_SYM) => {
                ensure_at!(span, arr.len() == 2, "invalid `not` predicate {}", arr);
                let pred = pred_from_form(arr.get(1)?, arr.span())?;
                Ok(Pred::Not(Box::new(pred)))
            }
            Val::Sym(OR_SYM) => {
                let preds: GResult<Vec<Pred>> = arr
                    .iter()
                    .skip(1)
                    .map(|val| pred_from_form(val, arr.span()))
                    .collect();
                Ok(Pred::Or(preds?))
            }
            Val::Sym(AND_SYM) => {
                let preds: GResult<Vec<Pred>> = arr
                    .iter()
                    .skip(1)
                    .map(|val| pred_from_form(val, arr.span()))
                    .collect();
                Ok(Pred::And(preds?))
            }
            _ => Ok(Pred::Form(form)),
        },
        form => Ok(Pred::Form(form)),
    }
}

//-------------------------------------------------------------------------------------------------
// matching codegen
//-------------------------------------------------------------------------------------------------

/*
we need to deal with four fundamental use-cases: let/def/with-global, field/const, matches?, and
when-let. everything else can be defined in terms of one of those macros. to summarise each
use-case's requirements:

    - (let/def/with-global) want to evaluate their initializer (let gs0 init), emit a series of
      basic definition forms (let a), followed by code to deconstruct the input and assign it
      to those definitions (= a x). a mismatch results in an immediate (bail).
        - the documentation for patterns is consistent with every one of the pattern's variables
          being in scope (but potentially uninitialized) during the evaluation of every predicate
          and every default-initializer.
    - (field/const) is almost identical, but the fields in question already exist and don't need
      to be defined before their assignment.
    - (matches?) can be implemented in a very convenient way by sacrificing some performance.
      generate the same code as (let/def/with-global), but nested within a (block ... #t), and
      have it call (finish-block #n) rather than failing.
        - i say "sacrificing performance" because, e.g., the array pattern (a b c) will actually
          index the target array three times, rather than just checking its length.
    - (when-let) is halfway between (matches?) and (let/def/with-global). it's basically a
      (block) with the (let pat src) codegen emitted at the start of the block, but with the same
      (finish-block #n) behaviour used by (matches?)
*/

#[derive(Copy, Clone)]
pub(crate) struct SetStrategy(pub(crate) PlaceStrategy, pub(crate) AssignStrategy);

#[derive(Copy, Clone)]
pub(crate) enum PlaceStrategy {
    //the local variable x
    Local,

    //the global variable (global 'x)
    Global,

    //the field @SymName:x
    Atsign(Sym),
}

#[derive(Copy, Clone)]
pub(crate) enum AssignStrategy {
    //assign results using (= place val)
    Set,

    //assign results using (push! place val)
    Push,

    //assign results using (= [place sym] val)
    Insert(Sym),

    //do not assign results at all
    Discard,
}

impl SetStrategy {
    fn codegen(&self, dst: &Arr, binding_name: Sym, to_assign: Val) -> GResult<()> {
        let place: Val = match self.0 {
            PlaceStrategy::Local => backquote!("~binding_name"),
            PlaceStrategy::Global => backquote!("(global '~binding_name)"),
            PlaceStrategy::Atsign(state_name) => {
                let qualified_str = format!("{}:{}", state_name, binding_name);
                let qualified_sym = glsp::sym(&qualified_str)?;
                backquote!("@~qualified_sym")
            }
        };

        let to_push: Val = match self.1 {
            AssignStrategy::Set => backquote!("(= ~place ~to_assign)"),
            AssignStrategy::Push => backquote!("(push! ~place ~to_assign)"),
            AssignStrategy::Insert(key) => backquote!("(= [~place ~key] ~to_assign)"),
            AssignStrategy::Discard => return Ok(()),
        };

        dst.push(to_push)?;
        Ok(())
    }
}

#[derive(Copy, Clone)]
pub(crate) enum MismatchStrategy {
    //on mismatch, call (finish-block the-sym #n)
    FinishBlock(Sym),

    //on mismatch, call (bail)
    Bail,
}

impl Matcher {
    fn names(&self, dst: &mut HashSet<Sym>, atsigns_only: bool) {
        match self {
            Matcher::Underscore => (),
            Matcher::Sym(name) => {
                if !atsigns_only {
                    dst.insert(*name);
                }
            }
            Matcher::AtsignSym(name) => {
                dst.insert(*name);
            }
            Matcher::Literal(_) => (),
            Matcher::Or(pats) | Matcher::Arr(pats) => {
                for pat in pats {
                    pat.names(dst, atsigns_only);
                }
            }
            Matcher::Index(vec) => {
                for (_, pat) in vec {
                    pat.names(dst, atsigns_only);
                }
            }
            Matcher::Opt(matcher, _) => matcher.names(dst, atsigns_only),
            Matcher::Rest(matcher) => matcher.names(dst, atsigns_only),
        }
    }
}

impl Pat {
    pub(crate) fn names(&self, dst: &mut HashSet<Sym>, atsigns_only: bool) {
        if let Some(at_name) = self.at {
            if !atsigns_only {
                dst.insert(at_name);
            }
        }

        self.matcher.names(dst, atsigns_only);
    }

    //push forms to dst which deconstruct the value stored in the local src_name, and assign it to
    //this Pat's bindings (using the given SetStrategy), executing the MismatchStrategy on failure.
    pub(crate) fn codegen(
        &self,
        src_name: Sym,
        dst: &Arr,
        set_strategy: SetStrategy,
        mismatch_strategy: MismatchStrategy,
    ) -> GResult<()> {
        match &self.matcher {
            Matcher::Underscore => (),
            Matcher::Sym(dst_name) | Matcher::AtsignSym(dst_name) => {
                set_strategy.codegen(dst, *dst_name, Val::Sym(src_name))?;
            }
            Matcher::Literal(literal_form) => match mismatch_strategy {
                MismatchStrategy::FinishBlock(block_name) => {
                    let to_push: Val = backquote!(
                        r#"
                            (unless (eq? ~src_name '~literal_form)
                              (finish-block ~block_name #n))
                        "#
                    );
                    dst.push(to_push)?;
                }
                MismatchStrategy::Bail => {
                    let err_msg = format!(
                        "literal pattern mismatch: expected {:?}, received ",
                        &literal_form
                    );
                    let to_push: Val = backquote!(
                        r#"
                            (ensure (eq? ~src_name '~literal_form) ~err_msg ~src_name)
                        "#
                    );
                    dst.push(to_push)?;
                }
            },
            Matcher::Or(pats) => {
                //we emit a (block ...) form which attempts to assign to each alternative in
                //turn, detecting failures with a MismatchStrategy::FinishBlock. on failure,
                //we need to set all of that alternative's bindings to #n before moving on to
                //the next.
                let outer_name = glsp::gensym();
                let outer_form: Root<Arr> = backquote!("(block ~outer_name)");

                for (i, pat) in pats.iter().enumerate() {
                    let inner_name = glsp::gensym();
                    let inner_form: Root<Arr> = backquote!("(block ~inner_name)");

                    pat.codegen(
                        src_name,
                        &inner_form,
                        set_strategy,
                        MismatchStrategy::FinishBlock(inner_name),
                    )?;

                    let to_push: Val = backquote!("(finish-block ~outer_name)");
                    inner_form.push(to_push)?;

                    outer_form.push(inner_form)?;
                    if i < pats.len() - 1 {
                        pat.codegen_nil(&outer_form, set_strategy)?;
                    }
                }

                let to_push: Val = match mismatch_strategy {
                    MismatchStrategy::FinishBlock(block_name) => {
                        backquote!("(finish-block ~block_name #n)")
                    }
                    MismatchStrategy::Bail => {
                        backquote!(r#"(bail "(or) pattern mismatch: no sub-patterns matched")"#)
                    }
                };
                outer_form.push(to_push)?;

                dst.push(outer_form)?;
            }
            Matcher::Arr(pats) => {
                //start by validating and counting the opt and rest sub-patterns.
                let mut normal_count = 0;
                let mut opt_count = 0;
                let mut seen_rest = false;
                for pat in pats {
                    match &pat.matcher {
                        Matcher::Opt(_, _) => {
                            ensure_at!(
                                pat.span,
                                !seen_rest,
                                "a (?) sub-pattern may not appear \
                                       after a .. subpattern"
                            );
                            opt_count += 1;
                        }
                        Matcher::Rest(_) => {
                            ensure_at!(pat.span, !seen_rest, "multiple .. sub-patterns");
                            ensure_at!(
                                pat.span,
                                pat.at.is_none(),
                                ".. patterns cannot be \
                                       combined with `at`"
                            );
                            seen_rest = true;
                        }
                        _ => {
                            ensure_at!(
                                pat.span,
                                opt_count == 0,
                                "(?) sub-patterns must all \
                                       appear together at the end of the array"
                            );
                            normal_count += 1;
                        }
                    }
                }

                //we can synthesise a minimum and maximum length, so there's no need to emit
                //a bounds check for each individual subpattern
                let min_len = normal_count;
                let max_len = if seen_rest {
                    None
                } else {
                    Some(normal_count + opt_count)
                };

                //emit an array type-check.
                let to_push: Val = match mismatch_strategy {
                    MismatchStrategy::FinishBlock(block_name) => {
                        backquote!("(unless (arr? ~src_name) (finish-block ~block_name #n))")
                    }
                    MismatchStrategy::Bail => {
                        backquote!(
                            r#"(ensure (arr? ~src_name) "array pattern mismatch: \
                                   expected an array, received {(type-of ~src_name)}")"#
                        )
                    }
                };
                dst.push(to_push)?;

                //emit the array length check.
                let len_form: Val = if opt_count > 0 || seen_rest {
                    let len_sym = glsp::gensym();

                    let to_push: Val = backquote!("(let ~len_sym (len ~src_name))");
                    dst.push(to_push)?;

                    Val::Sym(len_sym)
                } else {
                    backquote!("(len ~src_name)")
                };

                let test_form: Option<Val> = match (min_len, max_len) {
                    (0, None) => None,
                    (min_len, None) => Some(backquote!("(>= ~&len_form ~min_len)")),
                    (0, Some(max_len)) => Some(backquote!("(>= ~max_len ~&len_form)")),
                    (min_len, Some(max_len)) => {
                        if min_len == max_len {
                            Some(backquote!("(== ~min_len ~&len_form)"))
                        } else {
                            Some(backquote!("(>= ~max_len ~&len_form ~min_len)"))
                        }
                    }
                };

                if let Some(test_form) = test_form {
                    let to_push: Val = match mismatch_strategy {
                        MismatchStrategy::FinishBlock(block_name) => {
                            backquote!("(unless ~test_form (finish-block ~block_name #n))")
                        }
                        MismatchStrategy::Bail => {
                            let err_msg = match (min_len, max_len) {
                                (0, None) => unreachable!(),
                                (min_len, None) => {
                                    format!(
                                        "array pattern mismatch: expected a length of {} \
                                            or more, but the length was ",
                                        min_len
                                    )
                                }
                                (0, Some(max_len)) => {
                                    format!(
                                        "array pattern mismatch: expected a length of no more \
                                            than {}, but the length was ",
                                        max_len
                                    )
                                }
                                (min_len, Some(max_len)) if min_len == max_len => {
                                    format!(
                                        "array pattern mismatch: expected a length of \
                                            exactly {}, but the length was ",
                                        min_len
                                    )
                                }
                                (min_len, Some(max_len)) => {
                                    format!(
                                        "array pattern mismatch: expected a length of {} \
                                            to {}, but the length was ",
                                        min_len, max_len
                                    )
                                }
                            };

                            backquote!("(ensure ~test_form ~err_msg ~&len_form)")
                        }
                    };
                    dst.push(to_push)?;
                }

                //finally, recursively codegen each sub-pattern, usually by binding each
                //element to a local variable.
                let mut seen_rest = false;
                for (raw_i, pat) in pats.iter().enumerate() {
                    //for elements which appear after a ..rest, we want a negative index rather
                    //than a positive one.
                    let i: isize = if seen_rest {
                        -((pats.len() - raw_i) as isize)
                    } else {
                        raw_i as isize
                    };

                    match &pat.matcher {
                        Matcher::Opt(opt_matcher, None) => {
                            let opt_pat = Pat {
                                matcher: (**opt_matcher).clone(),
                                ..pat.clone()
                            };

                            //we need to clear each input binding to #n here if the element if
                            //absent, in case they shadow earlier names in the same pattern.
                            let else_do_form: Root<Arr> = backquote!("(do)");
                            opt_pat.codegen_nil(&else_do_form, set_strategy)?;

                            let element_name = glsp::gensym();
                            let then_do_form: Root<Arr> = backquote!(
                                r#"
                                (do
                                  (let ~element_name [~src_name ~i]))
                            "#
                            );
                            opt_pat.codegen(
                                element_name,
                                &then_do_form,
                                set_strategy,
                                mismatch_strategy,
                            )?;

                            let to_push: Val = backquote!(
                                r#"
                                (if (> ~&len_form ~i)
                                  ~then_do_form 
                                  ~else_do_form)
                            "#
                            );
                            dst.push(to_push)?;
                        }
                        Matcher::Opt(opt_matcher, Some(default_form)) => {
                            let opt_pat = Pat {
                                matcher: (**opt_matcher).clone(),
                                ..pat.clone()
                            };

                            let element_name = glsp::gensym();
                            let to_push: Val = backquote!(
                                r#"
                                (let ~element_name (if (> ~&len_form ~i)
                                  [~src_name ~i]
                                  ~default_form))
                            "#
                            );
                            dst.push(to_push)?;

                            opt_pat.codegen(element_name, dst, set_strategy, mismatch_strategy)?;
                        }
                        Matcher::Rest(rest_matcher) => {
                            seen_rest = true;
                            match (&**rest_matcher, pat.pred.is_some()) {
                                (Matcher::Underscore, false) => (),
                                (Matcher::Sym(rest_name), false) => {
                                    let offset = (normal_count + opt_count) - i;
                                    let to_assign: Val = backquote!(
                                        r#"
                                        [~src_name ~i : (- ~&len_form ~offset)]
                                    "#
                                    );

                                    set_strategy.codegen(dst, *rest_name, to_assign)?;
                                }
                                _ => {
                                    let rest_pat = Pat {
                                        matcher: (**rest_matcher).clone(),
                                        ..pat.clone()
                                    };

                                    //the generic pathway for rest patterns: assign a fresh
                                    //arr to each of the pattern's bindings. in a loop, match
                                    //the pattern repeatedly on each input element, with
                                    //a SetStrategy which pushes results to the destination name.
                                    let mut matcher_names = HashSet::new();
                                    rest_pat.names(&mut matcher_names, false);

                                    for name in matcher_names {
                                        let to_assign: Val = backquote!("(arr)");
                                        set_strategy.codegen(dst, name, to_assign)?;
                                    }

                                    let i_name = glsp::gensym();
                                    let element_name = glsp::gensym();
                                    let offset = (normal_count + opt_count) - i;
                                    let forn_form: Root<Arr> = backquote!(
                                        r#"
                                        (forn (~i_name ~i (- ~&len_form ~offset))
                                          (let ~element_name [~src_name ~i_name]))
                                    "#
                                    );

                                    let push_strategy =
                                        SetStrategy(set_strategy.0, AssignStrategy::Push);

                                    rest_pat.codegen(
                                        element_name,
                                        &forn_form,
                                        push_strategy,
                                        mismatch_strategy,
                                    )?;

                                    dst.push(forn_form)?;
                                }
                            }
                        }
                        Matcher::Underscore if pat.at.is_none() && pat.pred.is_none() => (),
                        Matcher::Sym(name) if pat.at.is_none() && pat.pred.is_none() => {
                            set_strategy.codegen(dst, *name, backquote!("[~src_name ~i]"))?;
                        }
                        _ => {
                            let element_name = glsp::gensym();
                            let to_push: Val = backquote!("(let ~element_name [~src_name ~i])");
                            dst.push(to_push)?;

                            pat.codegen(element_name, dst, set_strategy, mismatch_strategy)?;
                        }
                    }
                }
            }
            Matcher::Index(pairs) => {
                //check for a Rest sub-pattern, and if so, validate it.
                let mut rest_pat = None;
                for (i, &(_, ref pat)) in pairs.iter().enumerate() {
                    if let Matcher::Rest(rest_matcher) = &pat.matcher {
                        ensure_at!(
                            pat.span,
                            i == pairs.len() - 1,
                            "in [] patterns, .. sub-patterns must appear last"
                        );
                        ensure_at!(
                            pat.span,
                            pat.at.is_none(),
                            ".. patterns cannot be \
                                   combined with `at`"
                        );
                        rest_pat = Some(Pat {
                            matcher: (**rest_matcher).clone(),
                            ..pat.clone()
                        });
                    }
                }

                //if there's a Rest sub-pattern, we need to emit code to check that the input
                //value is a table, and if so shallow-clone it.
                let (coll_name, access_name) = if rest_pat.is_some() {
                    let coll_name = glsp::gensym();

                    let else_form: Root<Arr> = match mismatch_strategy {
                        MismatchStrategy::FinishBlock(block_name) => {
                            backquote!("(finish-block ~block_name #n)")
                        }
                        MismatchStrategy::Bail => {
                            backquote!(
                                r#"
                                (bail "index .. pattern mismatch: input is not a table")
                            "#
                            )
                        }
                    };

                    let to_push: Val = backquote!(
                        r#"
                        (let ~coll_name (if (tab? ~src_name)
                          (clone ~src_name)
                          ~else_form))
                    "#
                    );
                    dst.push(to_push)?;

                    (coll_name, glsp::sym("remove!")?)
                } else {
                    (src_name, ACCESS_SYM)
                };

                //process each non-Rest sub-pattern. either (fallibly?) index the collection,
                //or `remove!` an item from the collection (if a rest argument is present).
                for &(ref key, ref pat) in pairs {
                    match &pat.matcher {
                        Matcher::Opt(opt_matcher, None) => {
                            let opt_pat = Pat {
                                matcher: (**opt_matcher).clone(),
                                ..pat.clone()
                            };

                            //we need to clear each input binding to #n here if the element if
                            //absent, in case they shadow earlier names in the same pattern.
                            let else_do_form: Root<Arr> = backquote!("(do)");
                            opt_pat.codegen_nil(&else_do_form, set_strategy)?;

                            let element_name = glsp::gensym();
                            let then_do_form: Root<Arr> = backquote!(
                                r#"
                                (do
                                  (let ~element_name (~access_name ~coll_name '~key)))
                            "#
                            );
                            opt_pat.codegen(
                                element_name,
                                &then_do_form,
                                set_strategy,
                                mismatch_strategy,
                            )?;

                            let to_push: Val = backquote!(
                                r#"
                                (if (has? ~coll_name '~key)
                                  ~then_do_form 
                                  ~else_do_form)
                            "#
                            );
                            dst.push(to_push)?;
                        }
                        Matcher::Opt(opt_matcher, Some(default_form)) => {
                            let opt_pat = Pat {
                                matcher: (**opt_matcher).clone(),
                                ..pat.clone()
                            };

                            let element_name = glsp::gensym();
                            let to_push: Val = backquote!(
                                r#"
                                (let ~element_name 
                                  (if (has? ~coll_name '~key) 
                                    (~access_name ~coll_name '~key)
                                    ~default_form))
                            "#
                            );
                            dst.push(to_push)?;

                            opt_pat.codegen(element_name, dst, set_strategy, mismatch_strategy)?;
                        }
                        Matcher::Rest(_) => (),
                        _ => {
                            let else_form: Root<Arr> = match mismatch_strategy {
                                MismatchStrategy::FinishBlock(block_name) => {
                                    backquote!("(finish-block ~block_name #n)")
                                }
                                MismatchStrategy::Bail => {
                                    backquote!(
                                        r#"
                                        (bail "index pattern mismatch: missing key {'~key}")
                                    "#
                                    )
                                }
                            };

                            let element_name = glsp::gensym();
                            let to_push: Val = backquote!(
                                r#"
                                (let ~element_name 
                                  (if (has? ~coll_name '~key)
                                    (~access_name ~coll_name '~key)
                                    ~else_form))
                            "#
                            );
                            dst.push(to_push)?;

                            pat.codegen(element_name, dst, set_strategy, mismatch_strategy)?;
                        }
                    }
                }

                //finally, process the Rest sub-pattern.
                if let Some(rest_pat) = rest_pat {
                    match (&rest_pat.matcher, rest_pat.pred.is_some()) {
                        (Matcher::Underscore, false) => (),
                        (Matcher::Sym(rest_name), false) => {
                            set_strategy.codegen(dst, *rest_name, Val::Sym(coll_name))?;
                        }
                        _ => {
                            //the generic pathway for rest patterns: assign a fresh
                            //tab to each of the pattern's bindings. in a loop, match
                            //the pattern repeatedly on each input element, with
                            //a SetStrategy which inserts results into the destination tab.
                            let mut matcher_names = HashSet::new();
                            rest_pat.names(&mut matcher_names, false);

                            for name in matcher_names {
                                let to_assign: Val = backquote!("(tab)");
                                set_strategy.codegen(dst, name, to_assign)?;
                            }

                            let key_name = glsp::gensym();
                            let value_name = glsp::gensym();
                            let for_form: Root<Arr> = backquote!(
                                r#"
                                (for ~key_name in (keys ~coll_name)
                                  (let ~value_name [~coll_name ~key_name]))
                            "#
                            );

                            let insert_strategy =
                                SetStrategy(set_strategy.0, AssignStrategy::Insert(key_name));

                            rest_pat.codegen(
                                value_name,
                                &for_form,
                                insert_strategy,
                                mismatch_strategy,
                            )?;

                            dst.push(for_form)?;
                        }
                    }
                }
            }

            //Opt and Rest are only valid as the immediate child of an Arr or Index.
            Matcher::Opt(_, _) => bail_at!(self.span, "unexpected (?) pattern"),
            Matcher::Rest(_) => bail_at!(self.span, "unexpected .. pattern"),
        }

        if let Some(at_name) = self.at {
            set_strategy.codegen(dst, at_name, Val::Sym(src_name))?;
        }

        if let Some(ref pred) = self.pred {
            let pred_form = pred.codegen(src_name, self)?;

            let to_push: Val = match mismatch_strategy {
                MismatchStrategy::FinishBlock(block_name) => {
                    backquote!("(unless ~pred_form (finish-block ~block_name #n))")
                }
                MismatchStrategy::Bail => {
                    let err_msg = format!("predicate {} failed", pred);
                    backquote!("(ensure ~pred_form ~err_msg)")
                }
            };
            dst.push(to_push)?;
        }

        Ok(())
    }

    //push forms to dst which assign #n to each of this pattern's bindings
    fn codegen_nil(&self, dst: &Arr, set_strategy: SetStrategy) -> GResult<()> {
        let mut matcher_names = HashSet::new();
        self.names(&mut matcher_names, false);

        for name in matcher_names {
            set_strategy.codegen(dst, name, Val::Nil)?;
        }

        Ok(())
    }
}

impl Pred {
    //return a conditional form which tests this predicate against the value stored in src_name.
    pub(crate) fn codegen(&self, src_name: Sym, pat: &Pat) -> GResult<Val> {
        match self {
            Pred::Sym(pred_name) => {
                match pat.matcher {
                    Matcher::Underscore => (),
                    Matcher::Sym(_) => (),
                    _ => bail_at!(
                        pat.span,
                        "in order to use a symbol predicate like {}, the \
                                  pattern must be an underscore or a symbol",
                        pred_name
                    ),
                }

                Ok(backquote!("(~pred_name ~src_name)"))
            }
            Pred::Form(form) => Ok(form.clone()),
            Pred::Not(pred) => {
                let form: Val = pred.codegen(src_name, pat)?;
                Ok(backquote!("(not ~form)"))
            }
            Pred::Or(preds) => {
                let or_form: Root<Arr> = backquote!("(or)");
                for pred in preds {
                    let form: Val = pred.codegen(src_name, pat)?;
                    or_form.push(form)?;
                }
                Ok(Val::Arr(or_form))
            }
            Pred::And(preds) => {
                let and_form: Root<Arr> = backquote!("(and)");
                for pred in preds {
                    let form: Val = pred.codegen(src_name, pat)?;
                    and_form.push(form)?;
                }
                Ok(Val::Arr(and_form))
            }
        }
    }
}

impl Matcher {
    //for arr matchers only. get a minimum and maximum argument count (without performing
    //any validation)
    pub(crate) fn arg_limits(&self) -> (usize, Option<usize>) {
        if let Matcher::Arr(pats) = self {
            let mut normal_count = 0;
            let mut opt_count = 0;
            let mut seen_rest = false;

            for pat in pats {
                match &pat.matcher {
                    Matcher::Opt(_, _) => opt_count += 1,
                    Matcher::Rest(_) => seen_rest = true,
                    _ => normal_count += 1,
                }
            }

            let min_args = normal_count;
            let max_args = if seen_rest {
                None
            } else {
                Some(normal_count + opt_count)
            };

            (min_args, max_args)
        } else {
            panic!()
        }
    }
}
