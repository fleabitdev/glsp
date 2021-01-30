#![forbid(unsafe_code)]

use glsp::{stock_syms::*, DequeAccess, DequeOps, Engine, SymKind, Val};
use proc_macro::{TokenStream, TokenTree, TokenTree::Literal};
use std::char;
use std::collections::{hash_map::Entry, HashMap};
use std::fmt::Write;
use std::str::FromStr;

#[doc(hidden)]
#[proc_macro]
pub fn lazy_key(input: TokenStream) -> TokenStream {
    assert!(
        input.is_empty(),
        "the lazy_key!() macro expects no arguments"
    );
    TokenStream::from_str(r#"concat!(file!(), ":", line!(), ":", column!())"#).unwrap()
}

/**
Equivalent to [`'`](https://gamelisp.rs/std/quote).

The input must be a string literal which parses into a single value. The macro emits Rust code
which constructs that value, [deep&#8209;freezes](enum.Val.html#method.deep_freeze) it, and
returns it. For example, `quote!("(a 1)")` will allocate a new array by calling:

    arr![glsp::sym("a"), 1]

The first time that each `quote!()` is evaluated, the result is cached in the active
[`Runtime`](struct.Runtime.html). Any subsequent evaluations in that same
[`Runtime`](struct.Runtime.html) will return the cached value, without performing
any new allocations.

The result has a generic return type - it can be any type which implements
[`FromVal`](trait.FromVal.html). It should be bound to a local variable with a concrete type.

    let arr: Root<Arr> = quote!("(a b c)");
    let i: i64 = quote!("100");
    let val: Val = quote!("#((key0 #t) (key1 #f))");

In the unlikely event that conversion from [`Val`](enum.Val.html) to the destination type fails,
a panic will occur.

Unquoting (`~`) is not supported. If you need to interpolate local variables, use
[`backquote!()`](macro.backquote.html) instead.
*/

#[proc_macro]
pub fn quote(input: TokenStream) -> TokenStream {
    //wrangle input
    let text = parse_single_str_literal(input);

    //spin up a glsp Engine
    let engine = Engine::new();
    let output: String = engine
        .run(|| {
            //parse the input string
            let val = glsp::parse_1(&text, None).expect("quote!() received invalid glsp syntax");

            //emit rust code to create the input form.
            let mut builder = String::new();
            emit_val_for_quote(&mut builder, &val);

            //emit the lazy-initialization code
            let output = format!(
                r#"

            ::glsp::with_lazy_val(::glsp::lazy_key!(),
                || {{
                    let val = {};
                    val.deep_freeze();
                    val
                }},
                |val| ::glsp::FromVal::from_val(val).unwrap()
            )

        "#,
                builder
            );

            Ok(output)
        })
        .unwrap();

    TokenStream::from_str(&output).unwrap()
}

/**
Equivalent to [`` ` ``](https://gamelisp.rs/std/backquote).

The input must be a string literal which parses into a single value. The macro emits Rust code
which will construct that value and return it. For example, `backquote!("(a 1)")` could
potentially expand to:

    arr![glsp::sym("a"), 1]

Auto-gensyms, e.g. `name#`, are supported.

The result has a generic return type - it can be any type which implements
[`FromVal`](trait.FromVal.html). It should be bound to a local variable with a concrete type.

    let arr: Root<Arr> = backquote!("(a b c)");
    let i: i64 = backquote!("100");
    let val: Val = backquote!("#((key0 #t) (key1 #f))");

If the input contains any unquoted forms (typically using the `~` abbreviation), each
unquoted form must be a symbol which names a local variable in the same scope as the
`backquote!()` invocation. That local variable must implement the [`IntoVal`
trait](trait.IntoVal.html). It's converted into a [`Val`](enum.Val.html) which is
interpolated into the output.

Local variables can be borrowed by typing `~&var_name`. This prevents the variable
from being consumed by the `backquote!()` invocation. If `T` is the variable's type,
its shared reference `&T` must implement the [`IntoVal` trait](trait.IntoVal.html).

Local variables can be simultaneously unquoted and splayed with `~..`. In that case, the borrowed
form of the local variable must belong to a type which implements [`Splay`](trait.Splay.html).

If any of the [`IntoVal`](trait.IntoVal.html) conversions fail, the generated code will panic.
For a non-panicking version of this macro, use [`try_backquote!()`](macro.try_backquote.html).

    let c = 100_u64;
    let d = [200_u16, 250];
    let e = vec![275.0_f32, 287.5];
    let val: Val = backquote!(r#"
      (a "b" ~c ~..d ~&e)
    "#);
    println!("{}", val); //prints (a "b" 100 200 250 275.0 287.5)

This macro can be more convenient than using [`arr![]`](macro.arr.html) to construct
complicated nested forms. It's particularly useful when implementing GameLisp macros in Rust.
*/

#[proc_macro]
pub fn backquote(input: TokenStream) -> TokenStream {
    let mut output = try_backquote(input);
    output.extend(TokenStream::from_str(".unwrap()").unwrap());

    output
}

/**
A non-panicking version of [`backquote!()`](macro.backquote.html).

Returns a generic [`GResult<T>`](type.GResult.html), where `T` implements
[`FromVal`](trait.FromVal.html).
*/

#[proc_macro]
pub fn try_backquote(input: TokenStream) -> TokenStream {
    //wrangle input
    let text = parse_single_str_literal(input);

    //spin up a glsp Engine
    let engine = Engine::new();
    let output: String = engine
        .run(|| {
            //parse the input string
            let val =
                glsp::parse_1(&text, None).expect("backquote!() received invalid glsp syntax");

            //recursively emit rust code to create the input form. this is easier than it sounds:
            //  - #n, bools, ints, flos and chars are straightforward (e.g. #n emits "Val::Nil")
            //  - syms use the kind() method to check whether they're a StockSym, then they emit an
            //    appropriate call to glsp::sym or Sym::from_u32
            //  - most arrs can just emit the arr![] macro
            //      - backquote increases our nesting level
            //      - unquote and auto-gensym have no special handling if the nesting level is non-zero
            //      - name# emits a glsp::gensym_with_tag() just before the constructor
            //      - otherwise, ~name emits "name.into_val()", ~..name emits "..name.into_val()",
            //        ~&name emits "(&name).into_val()", and any other unquoted form is invalid
            //      - ~..name is invalid except as the immediate child of an arr![] invocation
            //  - tabs emit tab!{} for convenience
            //  - strs are constructed from a rust string literal with glsp::str_from_rust_str
            //  - no other types could have been parsed from the input string, since they don't
            //    have a text representation
            //
            //in order to resolve to a single GResult<Val>, we nest everything within curly braces,
            //define a closure which explicitly returns GResult<Val>, and mark any to_val()
            //invocations with ?

            let mut builder = String::new();
            let mut gensyms = HashMap::new();
            let mut gensym_counter = 0_usize;
            emit_val_for_backquote(
                &mut builder,
                &mut gensyms,
                &mut gensym_counter,
                &val,
                0,
                false,
            );

            let mut gen_vars = String::new();
            for (sym_name, var_name) in gensyms {
                write!(
                    &mut gen_vars,
                    "let {} = glsp::gensym_with_tag({:?}).unwrap();",
                    var_name,
                    &sym_name[0..sym_name.len() - 1]
                )
                .unwrap()
            }

            let output = format!(
                r#"
            {{
                let secret_backquote_closure = || -> ::glsp::GResult<::glsp::Val> {{
                    {}
                    Ok({})
                }};

                match secret_backquote_closure() {{
                    Ok(val) => ::glsp::FromVal::from_val(&val),
                    Err(err) => ::glsp::GResult::Err(err)
                }}
            }}
        "#,
                gen_vars, builder
            );

            Ok(output)
        })
        .unwrap();

    TokenStream::from_str(&output).unwrap()
}

fn emit_val_for_quote<T: Write>(dst: &mut T, val: &Val) {
    match *val {
        Val::Nil => write!(dst, "::glsp::Val::Nil").unwrap(),
        Val::Bool(b) => write!(dst, "::glsp::Val::Bool({:?})", b).unwrap(),
        Val::Int(i) => write!(dst, "::glsp::Val::Int({:?})", i).unwrap(),
        Val::Flo(f) => write!(dst, "::glsp::Val::Flo({:?})", f).unwrap(),
        Val::Char(c) => write!(dst, "::glsp::Val::Char({:?})", c).unwrap(),
        Val::Sym(s) => match s.kind() {
            SymKind::StockSpecial | SymKind::StockKeyword | SymKind::StockTransform => write!(
                dst,
                "::glsp::Val::Sym(::glsp::Sym::from_u32({:?}))",
                s.to_u32()
            )
            .unwrap(),
            SymKind::Normal => write!(
                dst,
                "::glsp::Val::Sym(::glsp::sym({:?}).unwrap())",
                s.name()
            )
            .unwrap(),
            SymKind::Gensym => unreachable!(),
        },
        Val::Str(ref st) => {
            write!(dst, "::glsp::Val::Str(::glsp::str_from_rust_str({:?}))", st).unwrap()
        }
        Val::Tab(ref tab) => {
            write!(dst, "::glsp::Val::Tab( ::glsp::tab! {{ ").unwrap();
            let len = tab.len();
            for (i, (key, value)) in tab.entries().iter().enumerate() {
                write!(dst, "(").unwrap();
                emit_val_for_quote(dst, &key);
                write!(dst, ", ").unwrap();
                emit_val_for_quote(dst, &value);
                write!(dst, ")").unwrap();

                if i < len - 1 {
                    write!(dst, ", ").unwrap();
                }
            }
            write!(dst, " }} )").unwrap();
        }
        Val::Arr(ref ar) => {
            if ar.len() == 0 {
                write!(dst, "::glsp::Val::Arr(::glsp::arr())").unwrap();
            } else {
                write!(dst, "::glsp::Val::Arr( ::glsp::arr! [ ").unwrap();
                let len = ar.len();
                for (i, item) in ar.iter().enumerate() {
                    emit_val_for_quote(dst, &item);

                    if i < len - 1 {
                        write!(dst, ", ").unwrap();
                    }
                }
                write!(dst, " ] )").unwrap();
            }
        }
        Val::GIter(_)
        | Val::RFn(_)
        | Val::Obj(_)
        | Val::Class(_)
        | Val::GFn(_)
        | Val::Coro(_)
        | Val::RData(_) => {
            unreachable!()
        }
    }
}

fn emit_val_for_backquote<T: Write>(
    dst: &mut T,
    gensyms: &mut HashMap<String, String>,
    gensym_counter: &mut usize,
    val: &Val,
    nesting: usize,
    can_splay: bool,
) {
    const GENSYM_PREFIX: &str = "backquote_var_";

    match *val {
        Val::Tab(ref tab) => {
            write!(dst, "::glsp::Val::Tab( ::glsp::tab! {{ ").unwrap();
            let len = tab.len();
            for (i, (key, value)) in tab.entries().iter().enumerate() {
                write!(dst, "(").unwrap();
                emit_val_for_backquote(dst, gensyms, gensym_counter, &key, nesting, false);
                write!(dst, ", ").unwrap();
                emit_val_for_backquote(dst, gensyms, gensym_counter, &value, nesting, false);
                write!(dst, ")").unwrap();

                if i < len - 1 {
                    write!(dst, ", ").unwrap();
                }
            }
            write!(dst, " }} )").unwrap();
        }
        Val::Arr(ref ar) => {
            if ar.len() == 0 {
                write!(dst, "::glsp::Val::Arr(::glsp::arr())").unwrap();
                return;
            }

            let first = ar.get::<Val>(0).unwrap();

            if first == Val::Sym(UNQUOTE_SYM) && nesting == 0 {
                assert!(ar.len() == 2, "invalid unquote in backquote!() macro");

                let second = ar.get::<Val>(1).unwrap();
                match second {
                    Val::Sym(sym) => {
                        let full_name = sym.name();
                        let name = if let Some(stripped) = full_name.strip_prefix('&') {
                            stripped
                        } else {
                            &full_name[..]
                        };

                        assert!(is_valid_identifier(name), "invalid identifier {}", name);
                        assert!(!name.starts_with(GENSYM_PREFIX), "unquoted an auto-gensym");

                        if full_name.starts_with('&') {
                            write!(dst, "::glsp::IntoVal::into_val(&{})?", name).unwrap();
                        } else {
                            write!(dst, "::glsp::IntoVal::into_val({})?", sym).unwrap();
                        }
                    }
                    Val::Arr(ar2)
                        if ar2.len() == 2 && ar2.get::<Val>(0).unwrap() == Val::Sym(SPLAY_SYM) =>
                    {
                        let splayee = ar2.get::<Val>(1).unwrap();
                        assert!(splayee.is_sym(), "invalid unquote in backquote!() macro");
                        assert!(can_splay, "in backquote!(), ~.. is not within an arr");

                        let sym = splayee.unwrap_sym();
                        assert!(
                            is_valid_identifier(&sym.name()),
                            "invalid identifier {}",
                            sym
                        );
                        assert!(
                            !sym.name().starts_with(GENSYM_PREFIX),
                            "unquoted an auto-gensym"
                        );

                        write!(dst, "..{}", sym).unwrap();
                    }
                    _ => panic!("invalid unquote in backquote!() macro"),
                }

                return;
            }

            let inner_nesting = match first {
                Val::Sym(BACKQUOTE_SYM) => nesting + 1,
                Val::Sym(UNQUOTE_SYM) => nesting.saturating_sub(1),
                _ => nesting,
            };

            write!(dst, "::glsp::Val::Arr( ::glsp::arr! [ ").unwrap();
            let len = ar.len();
            for (i, item) in ar.iter().enumerate() {
                emit_val_for_backquote(dst, gensyms, gensym_counter, &item, inner_nesting, true);

                if i < len - 1 {
                    write!(dst, ", ").unwrap();
                }
            }
            write!(dst, " ] )").unwrap();
        }
        Val::Sym(sym) if nesting == 0 && sym.name().ends_with('#') => {
            if let Entry::Vacant(entry) = gensyms.entry(sym.name().to_string()) {
                //it's safe for us to fudge a "unique" rust identifier by just incrementing a
                //counter, since the backquote!() macro only unquotes variables rather
                //than arbitrary expressions
                let var_name = format!("{}{}", GENSYM_PREFIX, *gensym_counter);
                *gensym_counter += 1;

                entry.insert(var_name);
            }

            let var_name: &String = gensyms.get(&*sym.name()).unwrap();

            write!(dst, "{}", var_name).unwrap();
        }
        _ => emit_val_for_quote(dst, val),
    }
}

fn is_valid_identifier(st: &str) -> bool {
    if st.is_empty() {
        false
    } else {
        let first = st.chars().next().unwrap();
        if first == '_' {
            st.len() >= 2
                && st[1..]
                    .chars()
                    .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
        } else if first.is_ascii_alphabetic() {
            st[1..]
                .chars()
                .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
        } else {
            false
        }
    }
}

/*
a TokenStream represents the output from rust's lexer. it provides no parsing support - just
tokenized utf-8 text.

the sanctioned way to parse rust tokens is the `syn` crate, but this is a large dependency with
multiple transitive dependencies - strongly prefer to avoid it, if possible.

as it so happens, quote!() and backquote!() only care about parsing rust's string literals, which
have a straightforward syntax. to avoid the `syn` dependency, we just roll up our sleeves and
implement a string-literal parser ourselves.
*/

fn parse_single_str_literal(input: TokenStream) -> String {
    //get the literal's text
    let tokens: Vec<TokenTree> = input.into_iter().collect();
    assert!(tokens.len() == 1, "expected a single string literal");

    let text = match tokens[..] {
        [Literal(ref lit)] => lit.to_string(),
        _ => panic!("expected a single string literal"),
    };
    let bytes = text.as_bytes();

    //inspecting the first two characters is sufficient to differentiate strings and raw strings
    //from other literals
    if text.starts_with("r\"") || text.starts_with("r#") {
        //a raw string literal
        let mut hashes = 0_usize;
        while bytes[hashes + 1] == b'#' {
            hashes += 1;
        }
        assert!(bytes[hashes + 1] == b'"' && bytes[bytes.len() - (hashes + 1)] == b'"');
        assert!(bytes[bytes.len() - hashes..].iter().all(|b| *b == b'#'));

        text[hashes + 2..bytes.len() - (hashes + 1)].to_string()
    } else if bytes[0] == b'"' {
        //a non-raw string literal
        assert!(bytes.len() >= 2 && bytes[0] == b'"' && bytes[bytes.len() - 1] == b'"');

        let mut dst = String::new();
        let mut i = 1;
        while i < bytes.len() - 1 {
            if bytes[i] == b'\\' {
                match bytes[i + 1] {
                    b'n' | b'r' | b't' | b'\\' | b'0' | b'\'' | b'"' => {
                        dst.push(match bytes[i + 1] {
                            b'n' => '\n',
                            b'r' => '\r',
                            b't' => '\t',
                            b'\\' => '\\',
                            b'0' => '\0',
                            b'\'' => '\'',
                            b'"' => '"',
                            _ => unreachable!(),
                        });
                        i += 2;
                    }
                    b'x' => {
                        dst.push(u8::from_str_radix(&text[i + 2..i + 4], 16).unwrap() as char);
                        i += 4;
                    }
                    b'u' => {
                        assert!(bytes[i + 2] == b'{');
                        let mut j = i + 3;
                        while bytes[j] != b'}' {
                            j += 1;
                        }

                        let num = u32::from_str_radix(&text[i + 3..j], 16).unwrap();
                        dst.push(char::from_u32(num).unwrap());

                        i = j + 1;
                    }
                    b'\r' | b'\n' => {
                        let mut j = i + 1;
                        while j < bytes.len() {
                            let ch = text[j..].chars().next().unwrap();
                            if !ch.is_whitespace() {
                                break;
                            }
                            j += ch.len_utf8();
                        }
                        i = j;
                    }
                    _ => panic!(),
                }
            } else {
                let ch = text[i..].chars().next().unwrap();
                dst.push(ch);
                i += ch.len_utf8();
            }
        }

        dst
    } else {
        panic!("expected a single string literal")
    }
}
