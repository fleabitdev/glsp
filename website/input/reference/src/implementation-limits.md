# Implementation Limits

There are several hard limits built in to GameLisp.

There cannot be more than 16,777,216 (`2^24`) distinct symbols. Running out of symbols is treated
as an unrecoverable error, similar to an out-of-memory condition: attempting to allocate additional
symbols will trigger a panic.

There can't be more than 256 simultaneous GameLisp function calls on the callstack. Attempting
to call the 257th function will trigger an error instead. This is because: 
- GameLisp implements recursion using Rust's callstack.
- A Rust stack overflow would abort the process.
- GameLisp function calls use up quite a lot of stack space.
- The default stack size for `*-pc-windows-msvc` targets is only one megabyte.

Rust currently seems to use up a very large amount of stack space in debug builds. If you try to 
run the `glsp` crate at `opt-level = 0`, you may still encounter stack overflows, even when there
are only a few dozen GameLisp function calls on the callstack.

Each "frame" (`fn` body or single toplevel form) may only contain 256 "registers" (parameters,
local variables, scratch registers or literals). Exceeding this limit will trigger an error.
This will usually only happen if you use macros to code-generate an extremely long function.
In that case, you can break up the function into multiple frames by wrapping each part of
its body in a separate `fn` form.

A `class` form may not contain more than 31 `state` or `state*` forms, including nested
states and states defined by mixins.

When a function call has more than 32 arguments, the 33rd and later arguments can't be splayed.

No more than 256 `Runtimes` may simultaneously exist within a single thread.

Each `Runtime` may not contain more than 8,388,606 (`2^23 - 2`) rooted objects. (This is the
combined total of objects which have been strongly rooted, with `Root`, and those which have been
weakly rooted, with `Gc`). There's also a soft limit on strongly-rooted objects, because they each 
consume a few nanoseconds of time whenever the garbage collector is invoked. If you're likely to 
require more than 100,000 strongly-rooted objects, consider storing that data in Rust rather than 
GameLisp.

There are a small number of ways that a `Root` or `Gc` may outlive its parent `Runtime`, or be 
assigned to a different `Runtime`. In either scenario, the only way that GameLisp can uphold 
memory safety is by immediately aborting the process! The most likely way you might do this 
accidentally is by either leaking a `Root` (using a function like `Box::leak` or `Rc::new`), 
or storing a `Root` in a `thread_local!` variable.
