# Introduction for Rust Programmers

Rust is my favourite programming language. It has impeccable performance, an expressive and
powerful type system, and a great community.

However, when it comes to game development, Rust has a few well-known problems. Compile times are
painfully slow, and the type system can sometimes feel rigid or bureaucratic. When adding a new
feature to a game, you'll often need to write code in a messy, fast, exploratory fashion - and in 
those cases, Rust can be a hindrance rather than a help.

GameLisp is a scripting language for Rust game development, designed while creating [The Castle 
on Fire](../tcof/). It complements Rust by being very different from it: the Yin to Rust's Yang. 
Unlike Rust, it's an interpreted, dynamically-typed language, comparable to Lua, Python or Ruby. 
When using GameLisp, your project can be rebuilt in milliseconds rather than minutes, and there's 
no static type-checker to step on your toes.

Of course, you could already get those benefits by binding an existing scripting language to your
Rust game project, using a crate like [rlua](https://crates.io/crates/rlua) for Lua or 
[pyo3](https://crates.io/crates/pyo3) for Python - so why add a new language into the mix? 
GameLisp has a few features which I think make it a better choice:

- GameLisp is a "Rust-first" scripting language. Integration of GameLisp code into a Rust project 
  is low-friction, high-performance, and completely memory-safe. Installation and distribution 
  are trivial (it's just a crate!). Language features like `match`, `for` and `struct` are
  designed to closely resemble their Rust counterparts.

- Garbage-collection pauses lead to dropped frames, and dropped frames lead to unhappy players. 
  GameLisp has an [unusual garbage collector](garbage-collection.md) which is designed to be 
  called once per frame, every frame, spreading out the workload so that it takes up a consistent 
  amount of runtime. In a [real-world game codebase](performance-figures.md#specimen-project), 
  GameLisp's garbage collector only takes up 0.1 milliseconds of frametime (that is, only 0.6% of 
  the available time budget).
    - More generally, [microbenchmarks](performance-figures.md#benchmarks) suggest that GameLisp's
      performance currently hovers somewhere between interpreted Lua and interpreted Python.

- As a Lisp dialect, GameLisp is extremely customizable. Its macro system allows you to define
  new syntax which is indistinguishable from the built-in syntax; create domain-specific
  languages; or even customize fundamental language features like `class` and `=`.
    - If you've used Lisp before and found that it wasn't to your taste, note that GameLisp 
      avoids [a few common pain points](introduction-for-lisp-programmers.md) from other Lisp 
      dialects.

- GameLisp follows the [Arc philosophy](http://www.paulgraham.com/arcll1.html) of being concise, 
  easy to type and easy to edit. The most common types and functions tend to be only a few
  characters in length: `str`, `obj?`, `=`, `defn`, `rev`. It includes several language features 
  intended to reduce boilerplate, such as iterators, coroutines, pattern-matching, and (of course) 
  macros. You could expect a thirty-line function in a Lua project to be a fraction of that length 
  in a GameLisp project.

- GameLisp has a novel object system, inspired by [Andy Gavin's writings on GOOL](https://all-things-andy-gavin.com/2011/03/12/making-crash-bandicoot-gool-part-9/), which is uniquely
  well-suited for game development. All objects are state machines: in the same way that your
  Rust codebase might use an `enum` or an `Option` to statically guarantee that its data
  model is never invalid, GameLisp code can use a [`state` form](state-machines.md) to explicitly 
  model the life cycle of its in-game entities. GameLisp's object model avoids inheritance (OOP's 
  biggest mistake!); instead, it encourages composition via 
  [classmacros](object-oriented-programming.md#classmacros) and [mixins](code-reuse.md#mixins).

If any of the above has whet your appetite, take a look at 
[Getting Started](overview.md#getting-started) for more information.

If you have previous experience with Lisp, you might be interested in the [Introduction for
Lisp Programmers](introduction-for-lisp-programmers.md).
