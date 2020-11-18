# Procedural Macros

The `glsp` crate defines a handful of procedural macros. In general, their purpose is to blur the 
line between GameLisp code and Rust code.


## `eval`

The [`eval!`] procedural macro takes a string literal which contains GameLisp source code, and 
executes it. You can interleave local variables into the evaluation by unquoting them with `~`.
Local variables are converted to and from GameLisp values using the [`ToVal`] and [`FromVal`] 
traits.

[`eval!`]: https://docs.rs/glsp/*/glsp/macro.eval.html
[`ToVal`]: https://docs.rs/glsp/*/glsp/trait.ToVal.html
[`FromVal`]: https://docs.rs/glsp/*/glsp/trait.FromVal.html

```rust
let width: i32 = 100;
let height: i32 = 200;
let mut area: i32 = 0;

let _: Val = eval!("(= ~area (* ~width ~height))")?;
```

You can split the string literal over several lines, and include embedded strings, using a
[raw string literal](https://doc.rust-lang.org/reference/tokens.html#raw-string-literals):

```rust
let y: usize = 20;
let _: Val = eval!(r#"
  (let x (* ~y 10))
  (prn "the value of y is {(/ x 10)}")
"#)?;
```

[`eval!`] is much faster than the [`eval`](../std/eval) and [`glsp::eval`] functions. While your 
crate is compiling, it fires up a GameLisp [`Runtime`] and uses it to compile the the literal 
string into GameLisp bytecode, which is lazily loaded into your own [`Runtime`] when the 
[`eval!`] is first executed. This means that, unlike the alternatives, [`eval!`] does not
need to compile any code when it's executed.

[`glsp::eval`]: https://docs.rs/glsp/*/glsp/fn.eval.html
[`Runtime`]: https://docs.rs/glsp/*/glsp/struct.Runtime.html

One small downside is that, because the code is expanded and compiled using a generic, empty 
[`Runtime`], it can't make use of any macros except those defined in the GameLisp standard 
library. However, it can still access your custom GameLisp functions, Rust functions and global 
variables as normal.

Because it performs bytecode serialization, the [`eval!`] macro is only available when the 
`"compiler"` [feature flag](feature-flags.md) is enabled.


## `quote`

The [`quote!`] macro is equivalent to the [`quote`](../std/quote) form. It takes a text description
of some GameLisp data, parses it at compile time, lazily allocates and deep-freezes it the first
time the [`quote!`] is executed, and then repeatedly returns the same data every time it's 
executed. This is sometimes more efficient than recreating the data from scratch.

Its return type is generic, so you should usually assign it to a variable with a concrete type,
such as [`Val`] or [`Root<Arr>`].

[`quote!`]: https://docs.rs/glsp/*/glsp/macro.quote.html
[`Val`]: https://docs.rs/glsp/*/glsp/enum.Val.html
[`Root<Arr>`]: https://docs.rs/glsp/*/glsp/struct.Arr.html

```rust
let ai_priorities: Root<Arr> = quote!(r#"
	(harvest-materials defend-self guard-allies build-structures)
"#);
creature.set("ai-priorities", ai_priorities)?;
```


## `backquote`

[`backquote!`] is equivalent to the [`backquote`](../std/backquote) form. It emits code to 
allocate a fresh, mutable copy of the specified GameLisp value, perhaps interleaving local 
variables into the output. It can be useful when implementing a GameLisp macro as a Rust 
function.

Local variables can be splayed, but otherwise it's not yet possible to evaluate arbitrary
Rust code within a [`backquote!`].

[`backquote!`]: https://docs.rs/glsp/*/glsp/macro.backquote.html

```rust
fn test_numbers_macro(attempt: Val) -> Val {
	let numbers: [i32; 4] = [36, -10, 59, 97];
	backquote!(r#"
		(cond 
		  ((eq? ~attempt '(~..numbers))
		    (prn "I have a bad feeling about this..."))
		  (else
		    (prn "Hint: one of the numbers is " (rand-select ~..numbers))))
	"#)
}
```

Unquoted local variables are converted into GameLisp values using the [`ToVal`] macro, which
has the potential to fail. [`backquote!`] will panic if the conversion fails. [`try_backquote!`]
is the non-panicking equivalent; it returns a [`GResult<T>`].

[`try_backquote!`]: https://docs.rs/glsp/*/glsp/macro.try_backquote.html
[`GResult<T>`]: https://docs.rs/glsp/*/glsp/type.GResult.html
