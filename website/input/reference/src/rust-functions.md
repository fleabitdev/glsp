# Rust Functions

As we mentioned several times throughout [Section 1](the-language.md), Rust functions can be bound
to GameLisp values. Their [primitive type](syntax-and-types.md#type-summary) is `rfn`.

In order to bind a Rust function to a GameLisp global variable, call [`glsp::bind_rfn`]:

```rust
use glsp::prelude::*;

fn print_them(arg: u8, tuple: (Sym, Num), st: &str) -> [f32; 3] {
	prn!("{:?} {:?} {:?}", arg, tuple, st);
	[10.5, 21.0, 42.0]
}

glsp::bind_rfn("print-them", rfn!(print_them))?;
```

```
(let result (print-them 1 '(my-sym 5.0) "hello"))
(prn result) ; prints (10.5 21.0 42.0)
```

Alternatively, you can use the [`glsp::rfn`] function to construct an `rfn` value directly.
They're represented by the [`RFn`] struct, which is a small `Copy` type, similar to [`Sym`].

```rust
let int_printer: RFn = glsp::rfn(rfn!(|n: i32| prn!("{}", n)));
arr.push(int_printer)?;
```

The [`glsp::rfn`] and [`glsp::bind_rfn`] functions accept an argument of type [`WrappedFn`]. This
should be constructed using the [`rfn!()` macro], which takes the name of a function, the full
path to a method, or a *non‑capturing* closure, and uses some type-system wizardry to produce 
a type-erased wrapper function.

```rust
glsp::bind_rfn("swap-bytes", rfn!(i32::swap_bytes))?;
```

[`glsp::rfn`]: https://docs.rs/glsp/*/glsp/fn.rfn.html
[`glsp::bind_rfn`]: https://docs.rs/glsp/*/glsp/fn.bind_rfn.html
[`RFn`]: https://docs.rs/glsp/*/glsp/struct.RFn.html
[`Sym`]: https://docs.rs/glsp/*/glsp/struct.Sym.html
[`WrappedFn`]: https://docs.rs/glsp/*/glsp/struct.WrappedFn.html
[`rfn!()` macro]: https://docs.rs/glsp/*/glsp/macro.rfn.html


## Type Conversions

Return types and parameters are automatically converted to and from GameLisp values.

Types which can be produced from GameLisp function arguments implement the [`MakeArg`] trait. 
You're not able to implement this trait directly for your own types, but it has a blanket 
implementation for any type which implements [`FromVal`].

Built-in [`MakeArg`] implementations include:

- All of Rust's primitive integer and floating-point types.

- Types which correspond to a GameLisp primitive type: `()`, `bool`, `char`, [`Sym`], [`RFn`], 
  [`Root<Arr>`].

- [`Num`], [`Deque`], [`Callable`], [`Iterable`].

- Tuples `(A, B)`, fixed-size vectors `[A; n]`, [`Vec`], [`VecDeque`] and [`SmallVec`], which are 
  collected from an array. For tuples and fixed-size vectors, the length of the input array must
  exactly match the length of the type.

- [`HashMap`] and [`BTreeMap`], which are collected from a table.

- [`String`], [`CString`], [`OsString`], [`PathBuf`], [`&str`], [`&Path`], all of which are copied
  from a GameLisp string. Note that types like [`&str`] and [`&Path`] copy the string into a temporary 
  variable, rather than borrowing its internal storage.

- References to types which can be pointed to by a [`Root`]: [`&Arr`], [`&Str`], [`&GFn`].

Return types which can be converted to a GameLisp value implement the [`IntoResult`] trait.
This trait is implemented for any `T` or [`GResult<T>`] where `T` implements [`ToVal`].
The built-in [`ToVal`] implementations are mostly the same types listed above.

[`MakeArg`]: https://docs.rs/glsp/*/glsp/trait.MakeArg.html
[`FromVal`]: https://docs.rs/glsp/*/glsp/trait.FromVal.html
[`ToVal`]: https://docs.rs/glsp/*/glsp/trait.ToVal.html
[`IntoResult`]: https://docs.rs/glsp/*/glsp/trait.IntoResult.html
[`GResult`]: https://docs.rs/glsp/*/glsp/type.GResult.html
[`Sym`]: https://docs.rs/glsp/*/glsp/struct.Sym.html
[`RFn`]: https://docs.rs/glsp/*/glsp/struct.RFn.html
[`Root<Arr>`]: https://docs.rs/glsp/*/glsp/struct.Arr.html
[`Num`]: https://docs.rs/glsp/*/glsp/enum.Num.html
[`Deque`]: https://docs.rs/glsp/*/glsp/enum.Deque.html
[`Callable`]: https://docs.rs/glsp/*/glsp/enum.Callable.html
[`Iterable`]: https://docs.rs/glsp/*/glsp/enum.Iterable.html
[`Root`]: https://docs.rs/glsp/*/glsp/struct.Root.html
[`&Arr`]: https://docs.rs/glsp/*/glsp/struct.Arr.html
[`&Str`]: https://docs.rs/glsp/*/glsp/struct.Str.html
[`&GFn`]: https://docs.rs/glsp/*/glsp/struct.GFn.html
[`Vec`]: https://doc.rust-lang.org/std/vec/struct.Vec.html
[`VecDeque`]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html
[`HashMap`]: https://doc.rust-lang.org/std/collections/struct.HashMap.html
[`BTreeMap`]: https://doc.rust-lang.org/std/collections/struct.BTreeMap.html
[`SmallVec`]: https://docs.rs/smallvec/*/smallvec/struct.SmallVec.html
[`OsString`]: https://doc.rust-lang.org/std/ffi/struct.OsString.html
[`CString`]: https://doc.rust-lang.org/std/ffi/struct.CString.html
[`String`]: https://doc.rust-lang.org/std/string/struct.String.html
[`PathBuf`]: https://doc.rust-lang.org/std/path/struct.PathBuf.html
[`&Path`]: https://doc.rust-lang.org/std/path/struct.Path.html
[`&str`]: https://doc.rust-lang.org/std/primitive.str.html


### Custom Type Conversions

It's possible to use your own *value* types (not reference types) as `rfn` return values or
parameters by implementing the [`ToVal`] and [`FromVal`] traits.

```rust
use glsp::prelude::*;

struct Coords {
	x: i32,
	y: i32
};

//a more efficient implementation is possible, as we'll see later
impl FromVal for Coords {
	fn from_val(val: &Val) -> GResult<Coords> {
		match val {
			Val::Obj(obj) => {
				let class: Root<Class> = glsp::global("Coords")?;
				ensure!(obj.is(&class), "expected Coords, received an obj");
				Ok(Coords {
					x: obj.get("x")?,
					y: obj.get("y")?
				})
			}
			val => bail!("expected Coords, received {}", val.a_type_name())
		}
	}
}

impl ToVal for Coords {
	fn to_val(&self) -> GResult<Val> {
		let class: Root<Class> = glsp::global("Coords")?;
		glsp::call(&class, &(self.x, self.y))
	}
}

fn offset(coords: Coords, dx: i32, dy: i32) -> Coords {
	Coords {
		x: coords.x + dx,
		y: coords.y + dy
	}
}

glsp::bind_rfn("offset", rfn!(offset))?;
```


## Optional and Rest Parameters

Parameters of type `Option<T>` are treated as optional. They must appear together, after
any non-optional parameters. When an optional parameter receives an argument, it's set to `Some`;
otherwise, it's set to `None`. 

The final parameter may be a `&[T]` or `&mut [T]` slice, in which case it will accept zero or 
more arguments.

```rust
fn example(non_opt: u8, opt: Option<u8>, rest: &[u8]) {
	prn!("{:?} {:?} {:?}", non_opt, opt, rest);
}

glsp::bind_rfn("example", rfn!(example))?;
```

```
(example)         ; error: too few arguments
(example 1000)    ; error: type mismatch
(example 1)       ; prints 1 None []
(example 1 2)     ; prints 1 Some(2) []
(example 1 2 3)   ; prints 1 Some(2) [3]
(example 1 2 3 4) ; prints 1 Some(2) [3, 4]
```


## Errors

To return an error from an `rfn`, you can simply set the function's return type to [`GResult<T>`],
which is an alias for `Result<T, GError>`.

The usual way to trigger a GameLisp error is using the macros [`bail!()`] and [`ensure!()`].
`bail` constructs a new `GError` and returns it. `ensure` tests a condition and calls `bail` 
when the condition is false. (The names of these macros are conventional in Rust error-handling
libraries, such as [`error-chain`] and [`failure`].)

If you need to create an error manually, you can use the [`error!()`] macro, or one of 
[`GError`]'s constructor methods. An arbitrary [`Error`] type can be reported as the cause of 
an error using the [`with_source`] method.

```rust
fn file_to_nonempty_string(path: &Path) -> GResult<String> {
	match std::fs::read_to_string(path) {
		Ok(st) => {
			ensure!(st.len() > 0, "empty string in file {}", path);
			Ok(st)
		},
		Err(io_error) => {
			let glsp_error = error!("failed to open the file {}", path);
			Err(glsp_error.with_source(io_error))
		}
	}
}
```

If a panic occurs within an `rfn`'s dynamic scope, the panic will be [caught] by the innermost 
`rfn` call and converted into a `GResult`. The panic will still print a message to stderr when 
it occurs, including a Rust stack-trace when the `RUST_BACKTRACE` environment variable is set.
If this is undesirable, you can override the default printing behaviour with a [custom panic hook].

[`bail!()`]: https://docs.rs/glsp/*/glsp/macro.bail.html
[`ensure!()`]: https://docs.rs/glsp/*/glsp/macro.ensure.html
[`error!()`]: https://docs.rs/glsp/*/glsp/macro.error.html
[`GResult`]: https://docs.rs/glsp/*/glsp/type.GResult.html
[`GResult<T>`]: https://docs.rs/glsp/*/glsp/type.GResult.html
[`GError`]: https://docs.rs/glsp/*/glsp/struct.GError.html
[`Error`]: https://doc.rust-lang.org/std/error/trait.Error.html
[`with_source`]: https://docs.rs/glsp/*/glsp/struct.GError.html#method.with_source
[caught]: https://doc.rust-lang.org/std/panic/fn.catch_unwind.html
[custom panic hook]: https://doc.rust-lang.org/std/panic/fn.set_hook.html
[`error-chain`]: https://docs.rs/error-chain/0.12.2/error_chain/
[`failure`]: https://docs.rs/failure/0.1.8/failure/


## `RFn` Macros

Both Rust functions and GameLisp functions can be used as GameLisp macros (although GameLisp 
functions are usually the much more convenient choice).

Within a Rust function, [`macro_no_op!()`] will create and return a special kind of `GError` 
which suppresses further expansion of the current macro. (Incidentally, this is also how the
[`macro-no-op`](../std/macro-no-op) built-in function works.)

This means that you can only use [`macro_no_op!()`] in a function which returns [`GResult`].

[`macro_no_op!()`]: https://docs.rs/glsp/*/glsp/macro.macro_no_op.html
