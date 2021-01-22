# RGlobal

As a Rust game developer, you're probably already aware of Rust's stern and disapproving 
attitude towards global variables.

In order to prevent unsafe mutation and unsafe multithreading, safe Rust is forced to completely
forbid global mutable variables. They need to be wrapped in a [`thread_local!`], a [`RefCell`], a 
[`Mutex`], a [`lazy_static!`], or something else to shield you from the shared mutable data.

[`thread_local!`]: https://doc.rust-lang.org/std/macro.thread_local.html
[`RefCell`]: https://doc.rust-lang.org/std/cell/struct.RefCell.html
[`Mutex`]: https://doc.rust-lang.org/std/sync/struct.Mutex.html
[`lazy_static!`]: https://docs.rs/lazy_static/

This is strictly necessary to uphold Rust's invariants, but it always makes global variables
much less convenient to use. Typical use of a [`thread_local!`] variable is not a pretty sight:

```rust
thread_local! {
    pub(crate) static LOG_FILE: RefCell<Option<File>> = RefCell::new(None);
}

fn log_str(text: &str) {
    LOG_FILE.with(|ref_cell| {
        let mut option = ref_cell.borrow_mut();
        if let Some(ref mut file) = *option {
            file.write_all(text.as_bytes()).ok();
        }
    })
}
```

Contrast the equivalent C code:

```c
thread_local FILE* log_file;

void log_str(const char* text) {
    if (log_file) {
        assert(fputs(text, log_file));
    }
}
```

When programming a game, you'll sometimes encounter a part of your engine which seems "naturally
global" - a texture manager, an audio mixer, a log file, an entity database. There's only ever 
going to be one of it, and it's only ever going to be accessed from a single thread, but Rust 
still forces you to keep it at arm's length.

You're given two options:
    
    - Use something like a [`lazy_static!`] [`RwLock`]. At minimum, this requires you to recite the
      incantation `NAME.read().unwrap()` every time you access the global object. This is 
      inconvenient, it makes the order of initialization/destruction less predictable, and it 
      carries a non&#8209;trivial performance cost.

    - Allocate your "global" object on the stack when your program starts up, and pass borrowed
      references down the callstack to any function which needs to access it. I would consider this 
      an anti&#8209;pattern in Rust game development - passing a context reference into most 
      function calls adds a lot of visual noise, and it sometimes causes borrow&#8209;checker 
      headaches.
        - This is why functions like [`glsp::sym`] are free functions, rather than being invoked
          as methods on some `&mut Glsp` "God object". An earlier version of the `glsp` crate
          did work that way, but it made the library much less pleasant to use.

[`RwLock`]: https://doc.rust-lang.org/std/sync/struct.RwLock.html
[`glsp::sym`]: https://docs.rs/glsp/*/glsp/fn.sym.html

If neither of these options seem appealing, GameLisp offers an alternative.


## `RGlobal`

An rglobal is a Rust object which is owned by the GameLisp [`Runtime`]. Predictably, the name 
stands for "Rust global". 

Unlike [`RData`], rglobals are singletons: for a given rglobal type, you can only store one 
instance of that type in each `Runtime`.
    
To define an rglobal, implement the [`RGlobal` trait] for one of your types, and then pass
an instance of that type to the [`glsp::add_rglobal`] function. Ownership will be transferred
to the active `Runtime`. If you later decide that you'd like to remove the rglobal from the
`Runtime` and take back ownership (perhaps because you're finished with the rglobal and you
want to drop it), you can call [`glsp::take_rglobal`].

While an rglobal of type `T` is registered with the active `Runtime`, you can temporarily
borrow it by calling [`T::borrow()`] or [`T::borrow_mut()`].

[`Runtime`]: https://docs.rs/glsp/*/glsp/struct.Runtime.html
[`RData`]: https://docs.rs/glsp/*/glsp/struct.RData.html
[`RGlobal` trait]: https://docs.rs/glsp/*/glsp/trait.RGlobal.html
[`glsp::add_rglobal`]: https://docs.rs/glsp/*/glsp/fn.add_rglobal.html
[`glsp::take_rglobal`]: https://docs.rs/glsp/*/glsp/fn.take_rglobal.html
[`T::borrow()`]: https://docs.rs/glsp/*/glsp/trait.RGlobal.html#method.borrow
[`T::borrow_mut()`]: https://docs.rs/glsp/*/glsp/trait.RGlobal.html#method.borrow_mut

```rust
struct Textures {
    by_name: HashMap<Sym, RRoot<Texture>>
}

impl RGlobal for Textures { }

fn init() {
    glsp::add_rglobal(Textures::new());
}

fn get_texture(name: Sym) -> GResult<RRoot<Texture>> {
    let textures = Textures::borrow();

    match textures.by_name.get(&name) {
        Some(texture) => Ok(RRoot::clone(texture)),
        None => bail!("texture {} does not exist", name)
    }
}
```

Notice that, unlike `RData`, we're able to store an `RRoot` in our rglobal without causing a
memory leak.

Dynamic checks are used to uphold Rust's aliasing rules - for example, it's an error to call 
[`glsp::take_rglobal`] or [`T::borrow_mut()`] for an rglobal which is currently borrowed. When the
[`Runtime`] is dropped, all of its rglobals will be dropped in the reverse order that they
were registered.

This is already a big improvement compared to [`lazy_static!`], but the real magic comes from
[argument type conversions](rfn.md#argument-conversions). When a function parameter is a
shared or mutable reference to a type which implements `RGlobal`, calling the function from 
GameLisp will automatically borrow that rglobal for the duration of the function call. If the 
`get_texture` function above was intended to be called from GameLisp, we could rewrite it like 
this:

```rust
impl Textures {
    fn get_texture(&self, name: Sym) -> GResult<RRoot<Texture>> {
        match self.by_name.get(&name) {
            Some(texture) => Ok(RRoot::clone(texture)),
            None => bail!("texture {} does not exist", name)
        }
    }
}

glsp::bind_rfn("get-texture", &Textures::get_texture)?;
```

Because the type of `&self` is `&Textures`, and the `Textures` type implements `RGlobal`, 
GameLisp will call `Textures::borrow()` to immutably borrow the `Textures` rglobal for the 
duration of each call to `get-texture`.

When an `rfn` parameter is borrowed from an rglobal, it doesn't consume an argument. You would
invoke this function from GameLisp by calling `(get-texture name)`, with only one argument.

A function may have multiple rglobal parameters:

```rust
impl Textures {
    fn draw_texture(
        &self,
        renderer: &mut Renderer,
        name: Sym,
        x: i32,
        y: i32
    ) -> GResult<()> {

        let texture = self.get_texture(name)?;
        renderer.draw(&texture.borrow(), x, y)
    }
}

glsp::bind_rfn("draw-texture", &Textures::draw_texture)?;
```

Assuming `Renderer` implements `RGlobal`, you would invoke the above function from GameLisp as 
`(draw-texture id x y)`.

This auto-borrowing is very convenient. It provides a pool of global variables which are 
"always available" to GameLisp and Rust code, with a minimum of fuss. 


## Symbol Caching

If you're finicky about performance, function calls like `glsp::global("texture-count")`
might make you nervous. That function will invoke `glsp::sym("texture-count")` every
time it's called, performing a hash-table lookup to convert the string into a symbol. Surely it
would be better to cache the symbol somewhere, and call `glsp::global(TEXTURE_COUNT_SYM)`
instead?

(In 90% of cases, the answer to that question is "no, it doesn't matter"... but the last 10% can 
be quite important!)

If you want to cache a symbol, the most convenient option is usually to store it in an rglobal.
This will ensure that you don't accidentally share symbols between one `Runtime` and another.
    
```rust
struct Textures {
    //...

    pub nearest_neighbour_sym: Sym,
    pub bilinear_sym: Sym,
    pub trilinear_sym: Sym
}

impl Textures {
    fn new() -> Textures {
        Textures {
            //...

            nearest_neighbour_sym: sym!("nearest-neighbour"),
            bilinear_sym: sym!("bilinear"),
            trilinear_sym: sym!("trilinear")
        }
    }
}
```

We provide the [`syms!`] macro to make this more straightforward. The macro defines a struct
for which every field is a Sym, with a constructor function named `new()` which initializes each
field by calling [`glsp::sym`].

[`syms!`]: https://docs.rs/glsp/*/glsp/macro.syms.html

```rust
syms! {
    pub (crate) struct Syms {
        pub(crate) nearest_neighbour: "nearest-neighbour",
        pub(crate) bilinear: "bilinear",
        pub(crate) trilinear: "trilinear"
    }
}

struct Textures {
    syms: Syms,

    //...
}

impl Textures {
    fn new() -> Textures {
        Textures {
            syms: Syms::new(),

            //...
        }
    }
}
```


## Multithreading

GameLisp is single-threaded, because multithreading is primarily a performance optimization. 
Nobody is writing multithreaded game code for its beauty or its convenience! GameLisp code simply 
[isn't fast enough](performance-figures.md) for multithreading to be worth the extra complexity it
would add to the language, so it's not supported.

That being said, GameLisp is designed to be cleanly embedded into a multithreaded Rust program.
You can spin up a separate isolated [`Runtime`] for each thread if you like (similar to a 
[Web Worker]), but it would be more typical to have one [`Runtime`] which lives on the main 
thread, with a few worker threads which only run Rust code.

[`Runtime`]: https://docs.rs/glsp/*/glsp/struct.Runtime.html
[Web Worker]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API

Let's imagine you have a `Clip` which stores a few seconds of PCM audio data, and a worker thread
which performs audio mixing in software. You want to load and manipulate `Clips` from your GameLisp
scripts, while still allowing the worker thread to access their PCM samples. A naive attempt 
would look something like this:
    
```rust
struct Clip {
    name: Sym,
    channels: Channels,
    samples: Vec<i16>
}

let clip = glsp::rdata(Clip::load("door-opening.wav"));
```

You'll run into a problem when you try to pass your `Clip` to your worker thread. Because the
`Clip` is owned by a GameLisp `Runtime`, you can only refer to it indirectly, using the types 
[`Root<RData>`], [`RRoot<Clip>`], `&Clip`, and `&mut Clip`. None of these types implement
[`Send`] or [`Sync`], so there's no way for you to access your `Clip`'s samples from another 
thread.

Thanks to fearless concurrency, there's an easy workaround. Simply wrap the shared parts of 
your struct in an [`Arc<T>`]:

[`Sym`]: https://docs.rs/glsp/*/glsp/struct.Sym.html
[`Root<RData>`]: https://docs.rs/glsp/*/glsp/struct.RData.html
[`RRoot<Clip>`]: https://docs.rs/glsp/*/glsp/struct.RRoot.html
[`Send`]: https://doc.rust-lang.org/std/marker/trait.Send.html
[`Sync`]: https://doc.rust-lang.org/std/marker/trait.Sync.html
[`Arc<T>`]: https://doc.rust-lang.org/std/sync/struct.Arc.html

```rust
struct Clip {
    name: Sym,
    samples: Arc<Samples>
}

struct Samples {
    channels: Channels,
    samples: Vec<i16>
}

impl Clip {
    fn play(&self, mixer: &Mixer) {
        mixer.play_samples(Arc::clone(&self.samples));
    }
}
```

Ideally, when writing a multithreaded game with GameLisp, your worker threads shouldn't know that 
GameLisp exists at all. Think of each worker thread's code as a small library written in pure Rust, 
for which your main thread is a client. In the example above, the `Mixer` could be thought of as
a library which mixes raw sample buffers, *not* a library which mixes `Clip`s specifically.


## Using GameLisp with ECS

Although GameLisp has some [philosophical differences](code-reuse.md#aside-why-not-ecs) with the
Entity-Component-System pattern (ECS), it's still possible for GameLisp and ECS to comfortably 
coexist in the same game.

The main obstacle is that most ECS libraries are pervasively multithreaded, but each GameLisp 
`Runtime` is strictly single-threaded. This means that GameLisp data can't be stored in 
components, and GameLisp scripts can't be executed by systems.

Thankfully, ECS libraries usually have a low-level API which can be used to look up an entity's
components dynamically, as long as no systems are currently executing. This can be used to 
implement a "pseudo-system" which runs GameLisp scripts once per frame, after all other entity 
processing is complete.

For example, with [`bevy`](https://bevyengine.org/) 0.4:

```
(defmixin Entity
  (field id) ; an rdata

  (prop position
    (get
      (ecs:position @id))
    (set (new-position)
      (ecs:position= @id new-position))))
```

```rust
#![feature(min_specialization)]

use bevy::{app::App, ecs::Entity};
use glsp::prelude::*;

struct Bevy(App);
impl RGlobal for Bevy { }

//a component
struct Position(f32, f32);

//the (ecs:position) function: access a Position from a glsp script
fn ecs_position(
    bevy: &Bevy,
    entity: &Entity
) -> (f32, f32) {

    let position = bevy.0.world.get::<Position>(*entity_id).unwrap();
    (position.0, position.1)
}

//the (ecs:position=) function: mutate a Position from a glsp script
fn ecs_set_position(
    bevy: &mut Bevy,
    entity_id: &Entity,
    (x, y): (f32, f32)
) {

    let mut position = bevy.0.world.get_mut::<Position>(*entity_id).unwrap();
    *position = Position(x, y);
}

fn main() {
    let runtime = Runtime::new();
    runtime.run(|| {

        //register accessor/mutator functions with glsp
        glsp::bind_rfn("ecs:position", &ecs_position)?;
        glsp::bind_rfn("ecs:position=", &ecs_set_position)?;

        //construct the ECS
        let mut app_builder = App::build();

        //...register your usual systems, and so on...

        //store the bevy App as an rglobal
        glsp::add_rglobal(Bevy(app_builder.app));

        //the main loop
        loop {
            //run bevy for a single step
            Bevy::borrow_mut().0.update();

            //now, you can execute glsp code for this step. global
            //functions like (ecs:position=) will automatically access a
            //particular Entity's components, stored within the bevy App
        }

        Ok(())
    }).unwrap();
}
```

I believe this approach would also be compatible with `specs`, the ECS used by the Amethyst
game engine.

With a little experimentation (and a little unsafe code!), it should also be possible to run 
GameLisp scripts from a thread-local system. For some games, this might be more the more 
ergonomic option.
