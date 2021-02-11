#![forbid(unsafe_code)]

use glsp::{arr, stock_syms::*, DequeAccess, DequeOps, Sym, Val};
use glsp_stdlib::Runtime;
use proc_macro::{Literal, TokenStream, TokenTree};
use proc_macro2::{Literal as Literal2, TokenStream as TokenStream2, TokenTree as TokenTree2};
use quote::quote;
use std::fmt::Write;
use std::path::PathBuf;
use std::str::FromStr;
use syn::{parse::Parser, punctuated::Punctuated, LitStr, Token};

/**
Pre-compiles GameLisp code and embeds it into the executable as a byte slice.

This macro is only available when the ["compiler" feature
flag](https://gamelisp.rs/reference/feature-flags.html#compiler) is enabled.

The input must be a comma-separated list of filepaths, which are looked up relative to the
[`CARGO_MANIFEST_DIR`](https://doc.rust-lang.org/cargo/reference/environment-variables.html).

```ignore
compile!["scripts/first.glsp", "scripts/second.glsp"]
```

When the `compile!` macro is expanded by rustc, it starts up a generic, empty
[`Runtime`](struct.Runtime.html); loads the specified files using
[`glsp::load_and_compile`](fn.load_and_compile.html); and embeds the resulting binary
into the program as a `&'static [u8]` which can be passed to
[`glsp::load_compiled`](fn.load_compiled.html).
*/

#[proc_macro]
pub fn compile(input: TokenStream) -> TokenStream {
    //wrangle input
    let parser = Punctuated::<LitStr, Token![,]>::parse_separated_nonempty;
    let args = parser
        .parse(input)
        .expect("compile![] expects a list of string literals");
    let files: Vec<String> = args.iter().map(|lit_str| lit_str.value()).collect();

    //check that each of the files exist
    let paths: Vec<PathBuf> = files
        .iter()
        .map(|st| PathBuf::from_str(st).unwrap())
        .collect();
    for path in &paths {
        assert!(
            path.is_file(),
            "nonexistent file {:?} passed to compile![]",
            path
        );
    }

    //generate a series of (load) forms: (load "a.glsp") (load "b.glsp")...
    let mut load_forms = String::new();
    for path in &paths {
        writeln!(&mut load_forms, "(load {:?})", path).unwrap();
    }

    //spin up a glsp Runtime and use it to compile those (load) forms into a Vec<u8>
    let runtime = Runtime::new();
    let bytes = runtime
        .run(|| {
            let (_, bytes) = glsp::load_and_compile_str(&load_forms, "compile-proc-macro")?;
            Ok(bytes)
        })
        .unwrap();

    //convert the bytes into a byte string literal. (there doesn't seem to be any better way to
    //embed arbitrary bytes into a Rust source file, but luckily it doesn't seem to slow down
    //the compiler.)
    TokenTree::Literal(Literal::byte_string(&bytes[..])).into()
}

/**
A convenient way to evaluate simple GameLisp code.

This macro is only available when the ["compiler" feature
flag](https://gamelisp.rs/reference/feature-flags.html#compiler) is enabled.

The input must be a single string literal which represents zero or more valid GameLisp forms.
Those forms are evaluated in order, and the final form's result is returned. The return type is
a generic [`GResult<T>`](type.GResult.html) for any `T` which implements
[`FromVal`](trait.FromVal.html) - you will usually need to bind it to a local variable with
an explicit type.

```ignore
# //this example can't be tested, due to intractable problems caused by
# //the macros importing names from ::glsp
let result: Root<Arr> = eval!(r#"
    (let n (+ 1 2 3 4))
    (arr n n n)
"#)?;
```

Rust's local variables can be captured, and/or mutated, using the
[`unquote`](https://gamelisp.rs/std/unquote) form (abbreviated as `~`).
They are copied into the form using the [`IntoVal` trait](trait.IntoVal.html).
If they're mutated using the [`=` form](https://gamelisp.rs/std/set), then
the local variables' values are updated when `eval!()` returns, using the
[`FromVal` trait](trait.FromVal.html).

```ignore
# //this example can't be tested, due to intractable problems caused by
# //the macros importing names from ::glsp
let input = 100_i64;
let mut output = "hello".to_string();
let assigned_but_not_read: f32;

let result: Val = eval!(r#"
    (= ~output (str ~input))
    (= ~assigned_but_not_read 100.0)
"#)?;
```

Some Rust collection types, such as tuples, slices, `Strings` and `HashMaps`, will allocate
a new GameLisp array, string or table when captured as a local variable. This is potentially
expensive, especially for large collections. Also, if the resulting collection is mutated,
those changes are not usually copied back into the local variable when `eval!()` returns.

```ignore
# //this example can't be tested, due to intractable problems caused by
# //the macros importing names from ::glsp
let rust_tuple = (60_u8, 120_u16, 240_u32);
let glsp_arr = arr![60_u8, 120_u16, 240_u32];

let _: Val = eval!(r#"
    (= [~rust_tuple 2] 480)
    (= [~glsp_arr 2] 480)
"#)?;

//rust collection types make a copy, but glsp collection types are passed in by reference
assert!(rust_tuple.2 == 240);
assert!(glsp_arr.get::<u32>(2)? == 480);
```

`eval!()` is much faster than GameLisp's own [`(eval)`](https://gamelisp.rs/std/eval) function,
because its body is compiled in advance. The first time each `eval!()` is executed, it has a
small lazy-initialization cost (usually less than 0.1 milliseconds for small inputs). On each
subsequent execution, the overhead is roughly one microsecond per `eval!()` call.

The input is macro-expanded using a generic, empty [`Runtime`](struct.Runtime.html) which does not
have access to any of your own macros. The only macros you can use in `eval!()` are those provided
by GameLisp's standard library, or those defined within the `eval!()` form itself.
*/

#[proc_macro]
pub fn eval(input: TokenStream) -> TokenStream {
    //wrangle input
    let lit: LitStr = syn::parse(input).expect(
        "the input to eval!() \
                                                must be a single string literal",
    );
    let text: String = lit.value();

    //spin up a glsp Runtime
    let runtime = Runtime::new();
    runtime
        .run(|| {
            glsp::seed_gensym();

            //parse the input string
            let forms = glsp::parse_all(&text, Some("eval-proc-macro"))
                .expect("eval!() received invalid glsp syntax");

            //make it mutable
            let forms: Vec<Val> = forms.iter().map(|v| v.deep_clone().unwrap()).collect();

            //we compile the forms into a single (fn (arg) ...) form. this means that, when executed
            //at runtime, it will return a Root<GFn> which we can stash in a lazy_val.

            //the argument to the fn is a newly-allocated arr where each item corresponds to an input
            //or output variable. after the fn returns, any output variables are updated with the
            //value in their corresponding arr item. (we can't recycle this arr because it would
            //prevent each eval!() from recursively calling itself.)
            let arg_gensym = glsp::gensym();
            let fn_form: Val = Val::Arr(arr![FN_SYM, arr![arg_gensym], ..forms]);

            //many macros will not handle (unquote x) forms transparently. for example, (= ~x 1)
            //will consider `unquote` to be an accessor function. in order to solve this, we perform
            //two transformation passes: the first detects (unquote x) forms and replaces them with
            //(access arg_gensym index) before macro expansion. the second runs after macro-expansion,
            //searches for (access arg_gensym index) and (access= arg_gensym index _) forms, and marks
            //those indexes as "accessed" and "mutated" respectively. this should catch most mutations
            //that we care about... (= ~x 1), (inc! ~x), etc.
            #[derive(PartialEq, Copy, Clone)]
            struct NameEntry {
                name: Sym,
                input: bool,
                output: bool,
            }

            fn pass0(val: &Val, names: &mut Vec<NameEntry>, arg_gensym: Sym) {
                if let Val::Arr(ref arr) = *val {
                    if arr.len() > 0 && arr.get::<Val>(0).unwrap().is_sym() {
                        let sym = arr.get::<Sym>(0).unwrap();

                        if sym == QUOTE_SYM {
                            //no-op
                        } else if sym == UNQUOTE_SYM {
                            assert!(arr.len() == 2, "invalid unquote form: {}", arr);
                            let identifier = arr.get::<Sym>(1).expect("invalid unquote form");
                            assert!(
                                is_valid_identifier(&identifier.name()),
                                "invalid unquoted identifier: {}",
                                identifier
                            );

                            for (i, entry) in names.iter_mut().enumerate() {
                                if entry.name == identifier {
                                    arr.clear().unwrap();
                                    arr.push(ACCESS_SYM).unwrap();
                                    arr.push(arg_gensym).unwrap();
                                    arr.push(i).unwrap();

                                    return;
                                }
                            }

                            names.push(NameEntry {
                                name: identifier,
                                input: false,
                                output: false,
                            });

                            arr.clear().unwrap();
                            arr.push(ACCESS_SYM).unwrap();
                            arr.push(arg_gensym).unwrap();
                            arr.push(names.len() - 1).unwrap();
                        } else {
                            for item in arr.iter() {
                                pass0(&item, names, arg_gensym);
                            }
                        }
                    } else {
                        for item in arr.iter() {
                            pass0(&item, names, arg_gensym);
                        }
                    }
                }
            }

            fn pass1(val: &Val, names: &mut Vec<NameEntry>, arg_gensym: Sym) {
                if let Val::Arr(ref arr) = *val {
                    if arr.len() >= 3
                        && arr.get::<Val>(0).unwrap().is_sym()
                        && arr.get::<Val>(1).unwrap() == Val::Sym(arg_gensym)
                        && arr.get::<Val>(2).unwrap().is_int()
                    {
                        let callee = arr.get::<Sym>(0).unwrap();
                        let index = arr.get::<i32>(2).unwrap() as usize;

                        if callee == ACCESS_SYM {
                            names[index].input = true;
                        } else if callee == SET_ACCESS_SYM {
                            names[index].output = true;
                        }
                    }

                    for item in arr.iter() {
                        pass1(&item, names, arg_gensym);
                    }
                }
            }

            let mut names = Vec::new();
            pass0(&fn_form, &mut names, arg_gensym);

            let fn_form =
                glsp::expand(&fn_form, None).expect("error when expanding eval!()'s input");

            pass1(&fn_form, &mut names, arg_gensym);

            //compile the fn form
            let (_, bytes) = glsp::load_and_compile_vals(&[fn_form], "eval-proc-macro")
                .expect("error when compiling eval!()'s input");
            let byte_string = TokenTree2::Literal(Literal2::byte_string(&bytes[..]));

            //emit the lazy-initialization code
            let mut vars_to_read = Vec::<TokenStream2>::new();
            let mut vars_to_write = Vec::<TokenStream2>::new();
            let mut indexes_to_write = Vec::<usize>::new();

            for (i, entry) in names.iter().enumerate() {
                let name = entry.name.name();

                if entry.input {
                    vars_to_read.push(name.parse().unwrap());
                }

                if entry.output {
                    if !entry.input {
                        vars_to_read.push("Val::Nil".parse().unwrap());
                    }
                    vars_to_write.push(name.parse().unwrap());
                    indexes_to_write.push(i);
                }
            }

            Ok(quote! {
                ::glsp::with_lazy_val(::glsp::lazy_key!(),
                    || ::glsp::load_compiled(#byte_string).unwrap(),
                    |__glsp_eval_val| {
                        let __glsp_eval_args = ::glsp::try_arr![#(&#vars_to_read),*]?;
                        let __glsp_eval_result: ::glsp::Val = match __glsp_eval_val {
                            ::glsp::Val::GFn(gfn) => ::glsp::call(gfn, &[__glsp_eval_args])?,
                            _ => ::std::panic!()
                        };
                        #(
                            #vars_to_write = match ::glsp::FromVal::from_val(
                                &__glsp_eval_args.get::<::glsp::Val>(#indexes_to_write).unwrap()
                            ) {
                                Ok(converted) => converted,
                                Err(err) => {
                                    let __glsp_eval_err = ::glsp::error!(
                                        "type mismatch when mutating variable {}",
                                        stringify!(#vars_to_write)
                                    );
                                    return Err(__glsp_eval_err.with_source(err))
                                }
                            };
                        )*
                        ::glsp::FromVal::from_val(&__glsp_eval_result)
                    }
                )
            }
            .into())
        })
        .unwrap()
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
