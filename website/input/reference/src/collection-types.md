# Collection Types

The collection types are [`Arr`] for arrays, [`Str`] for strings, and [`Tab`] for tables. Their 
API has a few quirks to bear in mind.

[`Arr`]: https://docs.rs/glsp/*/glsp/struct.Arr.html
[`Str`]: https://docs.rs/glsp/*/glsp/struct.Str.html
[`Tab`]: https://docs.rs/glsp/*/glsp/struct.Tab.html


## Error-Handling

We try to keep panics to an absolute minimum. Because there are a lot of things which can
potentially go wrong when interacting with a collection (out-of-bounds errors, failed type
conversions, mutating a borrowed collection), almost every collection method returns a
[`GResult`]. You'll need to make generous use of the `?` operator.

[`GResult`]: https://docs.rs/glsp/*/glsp/type.GResult.html


## Interior Mutability

All of the collection types are both aliasable and mutable - in Rust, this requires [interior 
mutability](https://doc.rust-lang.org/reference/interior-mutability.html). There are two ways 
this could potentially have worked:

- Allow the Rust programmer to explicitly lock the interior of the type, as though working with
  a [`RefCell`], and then mutate it freely - perhaps even directly accessing the internal storage.

- Build the API around "atomic" method calls which lock the type, do something to it, and then 
  unlock it before returning.

[`RefCell`]: https://doc.rust-lang.org/stable/std/cell/struct.RefCell.html

GameLisp previously chose the first solution, but it ended up cluttering the Rust code with
too many calls to `borrow()` and `borrow_mut()`, so we switched to the second option. The
second option also causes fewer lifetime problems in practice, since `borrow()` handles tend to be 
scoped for a longer lifetime than necessary.

The only methods which aren't completely atomic are those which [create a Rust iterator](#iteration) 
over a collection. If a collection is mutated while being iterated in Rust, an error will occur.

Because we don't allow the caller to create direct references into a collection's storage, none of
our collections can be indexed using Rust's `[]` syntax. You'll need to use the `get()` and
`set()` methods instead.


## Construction

There is no way to place an `Arr`, `Str` or `Tab` directly on the Rust stack. Instead, they're
always accessed indirectly via a `Root` smart pointer: `Root<Arr>`, `Root<Str>`, `Root<Tab>`.

[`Arr`]: https://docs.rs/glsp/*/glsp/struct.Arr.html
[`Str`]: https://docs.rs/glsp/*/glsp/struct.Str.html
[`Tab`]: https://docs.rs/glsp/*/glsp/struct.Tab.html
[`Root`]: https://docs.rs/glsp/*/glsp/struct.Root.html

New collections can be allocated using global functions in the `glsp` namespace, such as
[`glsp::arr`], [`glsp::str_from_iter`], and [`glsp::tab_with_capacity`].

[`glsp::arr`]: https://docs.rs/glsp/*/glsp/fn.arr.html
[`glsp::str_from_iter`]: https://docs.rs/glsp/*/glsp/fn.str_from_iter.html
[`glsp::tab_with_capacity`]: https://docs.rs/glsp/*/glsp/fn.tab_with_capacity.html

Alternatively, we provide convenience macros for constructing each type of collection, in the
same spirit as Rust's [`vec![]` macro](https://doc.rust-lang.org/stable/std/macro.vec.html).

[`arr!`] constructs a new array, returning it as a `Root<Arr>`:

[`arr!`]: https://docs.rs/glsp/*/glsp/macro.arr.html

```rust
arr![];
arr![elem; n]; //elem, repeated n times
arr![100i32, 20u8, "hello", arr![]]; //types are converted using the ToVal trait
arr![a, ..src, b]; //splays the contents of src using the Splay trait
```

[`str!`] has the same syntax as Rust's [`format!`] macro, but it returns a `Root<Str>`
rather than a `String`:

[`str!`]: https://docs.rs/glsp/*/glsp/macro.str.html
[`format!`]: https://doc.rust-lang.org/std/fmt/

```rust
str!("");
str!("hello, str");
str!("{}, {}", "hello", glsp::sym("str")?);
```

[`tab!`] constructs a new table from a number of key-value pairs. Optionally, it can
clone another table and then insert each of the key-value pairs into it, similar to Rust's
[struct update syntax]:

[`tab!`]: https://docs.rs/glsp/*/glsp/macro.tab.html
[struct update syntax]: https://doc.rust-lang.org/reference/expressions/struct-expr.html#functional-update-syntax

```rust
let empty_tab = tab! { };

let base_tab = tab! {
	(glsp::sym("a")?, 1),
	(glsp::sym("b")?, 2)
};

let extended_tab = tab! {
	(glsp::sym("b")?, 20),
	(glsp::sym("c")?, 30),
	..base_tab
};

assert!(extended_tab.len() == 3);
```

Conversion to a [`Val`] is fallible: the [`arr!`] and [`tab!`] macros will panic if you pass them 
something which can't be represented as a [`Val`], like `std::u32::MAX`. In the unlikely event
that you need to catch this type of error, the [`try_arr!`] and [`try_tab!`] macros are 
identical to their counterparts, except that they return a [`GResult<Root<_>>`].

[`Val`]: https://docs.rs/glsp/*/glsp/enum.Val.html
[`try_arr!`]: https://docs.rs/glsp/*/glsp/macro.try_arr.html
[`try_tab!`]: https://docs.rs/glsp/*/glsp/macro.try_tab.html
[`GResult<Root<_>>`]: https://docs.rs/glsp/*/glsp/type.GResult.html


## Deques

Within GameLisp, arrays and strings both support the `deque` interface, so that it's possible to
write a function which is generic over both types. The same is true for their Rust API.

The relevant traits are [`DequeOps`], [`DequeAccess`] and [`DequeAccessRange`]. You'll need to 
make sure those traits are in scope (perhaps by importing the 
[prelude](the-glsp-crate.md#the-prelude)) when working with arrays or strings.

The [`DequeAccess`] and [`DequeAccessRange`] implementations are generic over 
all of Rust's built-in integer types, so there's no need to convert indexes to `usize`. Negative 
indexes count backwards from the end of the deque.

```rust
let n: f32 = arr.get(30_u8)?;
arr.set(-1, n * 2.0)?;
```

### Type Erasure

If you need to define a variable, field or non-generic function argument which can be either a
`Root<Arr>` or a `Root<Str>`, you can use the [`Deque`] enum. It implements the [`DequeOps`],
[`DequeAccess`] and [`DequeAccessRange`] traits, so you don't need to unwrap it in order to 
manipulate its contents.

```rust
fn push_one_pop_one(deq: Deque, to_push: Val) -> GResult<Val> {
	deq.push(to_push)?;
	deq.pop_start()?;

	Ok(())
}
```

[`Deque`]: https://docs.rs/glsp/*/glsp/enum.Deque.html
[`DequeOps`]: https://docs.rs/glsp/*/glsp/trait.DequeOps.html
[`DequeAccess`]: https://docs.rs/glsp/*/glsp/trait.DequeAccess.html
[`DequeAccessRange`]: https://docs.rs/glsp/*/glsp/trait.DequeAccessRange.html

There are similar enums for GameLisp's other [abstract types](syntax-and-types.md#abstract-types):

- [`Num`] stores an `i32` or `f32`. It supports all of Rust's arithmetic operators, and
  also a subset of Rust's standard API for integer types, like the [`div_euclid()`] method.

- [`Callable`] stores those primitive types which can receive a GameLisp function call.

- [`Expander`] stores those primitive types which can be used as a macro expander.

- [`Iterable`] stores those primitive types which can be used to construct a [`GIter`].

[`Num`]: https://docs.rs/glsp/*/glsp/enum.Num.html
[`div_euclid()`]: https://doc.rust-lang.org/std/primitive.i32.html#method.div_euclid
[`Callable`]: https://docs.rs/glsp/*/glsp/enum.Callable.html
[`Expander`]: https://docs.rs/glsp/*/glsp/enum.Expander.html
[`Iterable`]: https://docs.rs/glsp/*/glsp/enum.Iterable.html
[`GIter`]: https://docs.rs/glsp/*/glsp/struct.GIter.html


## Iteration

Collections provide three separate iteration methods. 

`iter()` is an infallible iterator which returns the collection's natural type: `Val` for `Arr`, 
`char` for `Str` and `(Val, Val)` for `Tab`.

`iter_to<T>()` performs type conversion from its natural type to `T`. Because type conversion is
fallible, the iterator's `Item` type is [`GResult<T>`], so each item will need to be individually
unwrapped.

[`GResult<T>`]: https://docs.rs/glsp/*/glsp/type.GResult.html

```rust
let mut counter = 0u32;
for n in arr.iter_to::<u32>() {
	counter += n?;
}
```

`giter()` allocates and returns a new GameLisp iterator, `Root<GIter>`, over the collection.

[`Root<GIter>`]: https://docs.rs/glsp/*/glsp/struct.GIter.html

Due to some limitations in Rust's type system, it's not possible to call iteration methods on a
`Tab` directly. Instead, you'll need to use the [`entries()`] adapter method: for example,
[`tab.entries().iter()`] or [`tab.entries().keys_to::<u32>()`].

[`entries()`]: https://docs.rs/glsp/*/glsp/struct.Tab.html#method.entries
[`tab.entries().iter()`]: https://docs.rs/glsp/*/glsp/struct.TabEntries.html#method.iter
[`tab.entries().keys_to::<u32>()`]: https://docs.rs/glsp/*/glsp/struct.TabEntries.html#method.keys_to

## Cloning

The `clone()` method name has already been claimed by Rust. If you call `clone()` for a 
`Root<Arr>`, it will create a new `Root` which refers to the same array, rather than cloning the 
array itself.

When you need to clone the *contents* of a collection, the correct methods are `shallow_clone()` 
and `deep_clone()`.
