# RClass

By default, from the perspective of GameLisp scripts, each `rdata` is a black box. GameLisp code
can query whether a particular value is an `rdata`, receive `rdata` from function calls,
pass `rdata` as function arguments... and that's about it!

If you prefer your scripting APIs to be a little more object-oriented, you can register an 
`RClass` ("Rust&nbsp;class") for any Rust type. When an `rdata` has an associated `RClass`,
this will cause the `rdata` to behave like a GameLisp [object](object-oriented-programming.md).
It can have methods and properties; its type can be queried using the 
[`is?` function](../std/is-p); and it has full support for [operator 
overloading](structs.md#operator-overloading).

To register an `RClass`, use the [`RClassBuilder` struct]. The builder's api is designed to
vaguely resemble a [`defclass` form]. We saw this API used [earlier](rust-bindings.md#gamelisp), 
for our `Texture` struct:

```rust
RClassBuilder::<Texture>::new()
    .met("width", &Texture::width)
    .met("height", &Texture::height)
    .build();
````

You can register an `RClass` for any `'static` Rust type, including types defined by external 
crates. The only restriction is that each `RClass` must have a unique name. If you have two 
types, one named `audio::Clip` and one named `video::Clip`, they can't both be named `Clip`.

```rust
glsp::bind_rfn("Command", &Command::new::<String>)?;

RClassBuilder::<Command>::new()
    .met("args", &|command: &mut Command, rest: Rest<String>| {
        command.args(rest);
    })
    .met("status", &Command::status)
    .build();
```

```
(let command (Command "ls"))
(ensure (is? command 'Command))
(.args command "-l" "-a")

; spawn the process and wait for it to complete
(.status command)
```

[`RClassBuilder` struct]: https://docs.rs/glsp/*/glsp/struct.RClassBuilder.html
[`defclass` form]: object-oriented-programming.md#fundamentals


## Weak Roots

Generally speaking, it's fine to move any `'static` type onto the GameLisp heap. However, 
there's one exception.

When a `Root` smart pointer is pointing to an object, it will entirely prevent that object from 
being deallocated. This means that if any `Root` pointers are moved onto the garbage-collected heap
(for example, by storing a `Root` in a struct which is passed to `glsp::rdata`), memory leaks 
will occur. The `Val` and `RRoot` types may contain `Roots`, so those types should also be kept 
off the heap.

If you need a pointer from one heap allocation to another, you should use [`Gc`] instead.
`Gc` is a weak reference; it doesn't prevent its pointee from being deallocated, and so it
won't cause any memory leaks.

Unfortunately, `Gc` does require a little bit of supervision. Because `Gc` pointers are
weakly-rooted, this means that the object they're pointing to could be deallocated at any
time! To prevent this from happening, you'll need to:

- Use [`RClassBuilder::trace`] to specify how the garbage collector should visit each of the 
  `Gc` pointers owned by your Rust type.

- Call [`glsp::write_barrier`] when an instance of your Rust type has been mutated, so that
  the garbage collector can check whether any new `Gc` pointers have been added to it.

```rust
//don't do this! 
struct Colliders {
    array: Root<Arr>
}

impl Colliders {
    fn get(&self) -> Root<Arr> {
        Root::clone(&self.array)
    }

    fn replace(&mut self, new_array: Root<Arr>) {
        self.array = new_array;
    }
}

RClassBuilder::<Colliders>::new()
    .met("get", &Colliders::get)
    .met("replace!", &Colliders::replace)
    .build();
```

```rust
//instead, do this...
struct Colliders {
    array: Gc<Arr>
}

impl Colliders {
    fn get(&self) -> Root<Arr> {
        self.array.upgrade().unwrap()
    }

    fn replace(&mut self, new_array: Root<Arr>) {
        self.array = new_array.downgrade();
    }

    fn trace(&self, visitor: &mut GcVisitor) {
        visitor.visit(&self.array);
    }
}

RClassBuilder::<Colliders>::new()
    .met("get", &Colliders::get)
    .met(
        "replace!",
        &|colliders: RRoot<Colliders>, new_array: Root<Arr>| {
            colliders.borrow_mut().replace(new_array);
            glsp::write_barrier(&colliders.into_root());
        }
    )
    .trace(Colliders::trace)
    .build();
```

We also provide [`GcVal`] as a weakly-rooted alternative to `Val`, and [`RGc`] as a
weakly-rooted alternative to `RRoot`.

Make sure you don't get caught out when constructing an `rfn`! If your `rfn` is a Rust closure
which captures a `Root`, `Val` or `RRoot`, then you're effectively moving a `Root`
onto the GameLisp heap, which will cause the captured data to leak. (Of course, for an `rfn` 
which is bound to a global variable, a memory leak might be acceptable.)

```rust
let primes: Root<Arr> = arr![2, 3, 5, 7, 11];
primes.freeze();
let closure = move || { Root::clone(&primes) };

glsp::bind_rfn("primes", Box::new(closure))?;
```

[`Gc`]: https://docs.rs/glsp/*/glsp/struct.Gc.html
[`GcVal`]: https://docs.rs/glsp/*/glsp/struct.GcVal.html
[`RGc`]: https://docs.rs/glsp/*/glsp/struct.RGc.html
[`RClassBuilder::trace`]: https://docs.rs/glsp/*/glsp/struct.RClassBuilder.html#method.trace
[`glsp::write_barrier`]: https://docs.rs/glsp/*/glsp/fn.write_barrier.html
