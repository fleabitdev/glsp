# The Garbage Collector

Our garbage-collector is designed to be a good fit for a "typical" video game use-case:

- There is a large amount of long-lived memory (megabytes) which experiences very little turnover.

- All other allocations (a few dozen to a few hundred kilobytes per frame) are highly generational. 
  90% of them won't survive past one frame, and most of the rest won't survive past two frames.

- Control is manually yielded to the GC once per frame. The mutator is expected to do a fairly 
  consistent amount of work from one frame to the next. In exchange, the programmer can expect 
  each yield  point to take up a predictable amount of runtime, with no latency spikes.

	- Our goal is not to be perfectly consistent, but instead to be proportional to the 
	  amount of new allocation each frame - if the programmer allocates twice as many bytes 
	  this frame, then the GC is allowed to run for twice as long.

We currently approach this use-case by pairing a generational algorithm with a tri-colour 
incremental algorithm. (For a good summary of the prior art, see
[this page](http://wiki.luajit.org/New-Garbage-Collector).)

Each time the programmer yields to the GC by calling `(gc)`, that's a "step". Our algorithm is
divided into "cycles", made up of many steps. At the end of each cycle, we will have identified a 
new set of unreachable objects, which we arrange to be gradually deleted during the next cycle.


## The Young Generation

All new objects are placed in the young generation. When the write-barrier detects that a young 
object is cross-referenced by an old object, the young object is marked. At the start of each step, 
if a young object is pointed to by a root, that young object is also marked.

At the beginning of each step, we perform a full stop-the-world mark-and-sweep collection of the 
young generation. Any unreachable young objects (the vast majority!) are immediately freed. Any
reachable young objects are promoted to the old generation, coloured black.


## The Old Generation

Old objects can have one of four colours:

- White objects have not yet been visited this cycle
- Gray objects have been visited, but not traversed, this cycle
- Black objects have been traversed this cycle
- Ghost objects were proven unreachable at the end of a previous cycle; they're in the 
  process of being gradually freed

The write-barrier detects when a black object or young object is mutated to point to a white 
object, in which case it turns the white object gray. Likewise, at the start of each step, if
a root is found to point to a white object, that white object is turned gray.

Each step, we traverse a certain number of gray objects: we turn the gray object black, and when
it refers to any white object, we turn those white objects gray.

Each step, we also free a certain number of ghost objects.

Each cycle ends when there are no more gray objects available to traverse: all old objects are
black, white or ghosts. This means that all existing white objects have been proven to be
unreachable. We convert the white objects into ghost objects, the black objects into white 
objects, and start the next cycle.

In order to avoid a latency spike when performing this "colour change", the meaning of each
colour index changes between one cycle and the next. An object tagged as "colour 2" might be
considered a black object during one cycle, then a white object during the next cycle, without 
actually changing its colour index.

Note that the algorithm runs on a significant time-lag: although white objects at the end of a 
cycle have been proven unreachable, black objects are not necessarily still reachable! Unreachable
black objects will turn white at the end of the cycle, be converted into ghost objects at the end 
of the *next* cycle, and only actually freed at some point during the following cycle.


## Tuning

The number of ghost bytes and gray bytes processed each step is directly proportional to the 
number of new black bytes promoted from the young generation each step.

The constant of proportionality can be tuned by the programmer. They specify a constant `U`,
which must be at least 1.2, representing the ratio between long-lived allocations and the heap's
total memory usage. For example, with 5mb of long-lived allocations and `U = 1.5`, the heap will
hover around 7.5mb after its first cycle or two. 

(When there are few long-lived allocations, a cycle is not permitted to end until the heap size
exceeds 1mb, even if all gray objects have been processed.)

We anticipate that except for long-lived allocations, the heap will be evenly split between:
	
- Unreachable white objects from the previous cycle.

- Unreachable black objects which were promoted from the young generation this cycle, AND
  the remaining ghost objects from the previous cycle, which are deallocated at a rate 
  roughly matching the rate that young objects are promoted.

Let `B` be the size of the long-lived allocations. When we've finished promoting `(B * (U-1)/2)`
bytes of young objects, we also need to have traversed all of the long-lived bytes.

We can use this information to calculate `R`: the ratio between the number of promoted bytes each
step, and the number of long-lived bytes which we need to traverse that step.
	
	R = 2/(U - 1)

By default, `U = 1.5`, so `R = 4.0`. For each 100kb of black objects promoted from the young
generation, the collector will convert 400kb of gray objects into black objects.

Incidentally, although it's not tuneable, we also store the ratio W at the end of each cycle.
	
	W = [number of ghost bytes] / [number of surviving bytes]

When we produce 100kb of black objects (either promoted or traversed), we must also free
`W * 100`kb of ghost objects. The goal is that we will evenly spread out our ghost-object
freeing over the entire following cycle.


## Root Soundness

We make it very difficult to pass certain types (`Root`, `Sym`, `RFn`) from one `Runtime` to 
another. However, there's no way to forbid it entirely. `Box::leak`, and the `thread_local!` macro, 
both permit any `'static` type (like `Root`!) to be given an indefinite lifetime.

`Syms` and `RFns` will just cause harmless undefined behaviour or out-of-bounds panics when moved 
between `Runtimes`, but if a `Root` (or a `Gc` derived from it) is stored in a foreign `Heap`, 
that would cause use-after-free bugs in `"unsafe-internals"` mode. For example...
	
- Store a `Root<Arr>` from `Heap` A in a `thread_local` variable

- Call an rfn from Heap B. Access the `thread_local`, and return the `Root<Arr>` from the rfn

- Store the result of that rfn in a local variable within glsp (which will convert it to 
  a `Gc<Arr>`, and store it in a `Slot::Arr` on the stack, which isn't write-barriered)

- Mutate the `thread_local` using interior mutability, so that it no longer stores a 
  `Root<Arr>`

- Call another rfn which drops `Heap` A's Runtime, causing the `Arr` referred to by the 
  `Gc<Arr>` to be deallocated

- Execute an operation which dereferences the `Gc<Arr>` local variable, such as `[loc 0]`

We prevent this behaviour by aborting the process if a `Root` or `Gc` could outlive its
originating `Heap`. We have to abort rather than panicking, because panics in Rust are recoverable 
using `catch_unwind`. Specifically...
	
- If a `Heap` is dropped with at least one extant `Root`, we abort the process.

	- This means that the only way for a `Root` to outlive its Heap is for it to be converted 
	  into a `Gc`; since `Gc` is an undocumented type (which the user is completely forbidden
	  from accessing, for safety reasons), this can only happen within the `glsp-engine` crate.

- Whenever a `Root` is converted to a `Gc`, or a `Root` is created, cloned or dropped, if 
  the pointed-to object's originating `Runtime` ID differs from the active `Runtime` ID, we 
  abort the process.
	
	- If we didn't do this then we would need to screen every `Slot` that's passed to `vm.rs` 
	  as an argument or return value - it would be a lot more complex, while also probably 
	  leaking like a sieve. Checking the `Root`/`Gc` conversions is simple and safe.
	- This is only safe if there's no race condition which would allow the active `Runtime` 
	  to change between creating a `Gc` or `Slot` and storing it on the active `Heap`. This 
	  isn't currently plausible, but I suppose that could change in the future.

- Finally, the write barrier tests for a mismatch between the `Heap` on which a `Gc` is
  being stored, and the `Heap` to which the `Gc` points.

	- This is necessary for the case where you have an active `Heap` A, a `Root<Arr>` pointing
	  into `Heap` B, a `Root<GFn>` pointing into `Heap` A, and you call `the_arr.set(0, the_gfn)?`.
	  This won't be caught by the `Root`-to-`Gc` conversion check, because the `Root<GFn>` matches 
	  the active `Heap`, even though it doesn't match the `Heap` on which it's about to be stored.

I think I've covered everything, but security audits are always welcome!

Interestingly, these checks all seem to have very little performance impact in practice, perhaps
because they're just performing a highly predictable one-byte comparison of a value which would
have needed to be accessed anyway.


## Safe-Internals Mode

We create a garbage collector in safe Rust by making the `Gc` smart pointer into a thin
wrapper over an `Rc`. We break reference cycles by using interior mutability to clear some types 
when they become unreachable, rather than when they're actually dropped by Rust.

This cycle-breaking is achieved in the `Gc::free()` function. In `"unsafe-internals"` mode, this
is an actual deallocation. In safe mode, it uses internal mutability to remove `Gc` pointers
from *some* types. We can't clear all types, because that would require interior mutability to
be added to a number of types which don't need it, like `Bytecode`. However, it's safe to leave
many types as they are, because they can never produce a reference cycle:

- `GFns` only refer to a `Lambda` and some `Stays`.
- `Lambdas` only refer to the `Bytecode` generated with them during compilation.
- `Bytecodes` refer to any `Lambdas` they may create, and to their literal `Slots`.
	- The references between `Bytecodes` and `Lambdas` are tree-shaped; they can't form cycles.
	- Literal `Slots` may only refer to `Arrs`, `Strs` and `Tabs` (see `quote_to_node`).
- `Classes` only have their constant table, which is frozen before the `Class` itself exists.

Currently, the main downside of this approach is that it could potentially cause ghost objects to
be freed in a "chunky" fashion - that is, calling `Gc::free` for one object could potentially cause
a huge tree of objects to be deallocated all at once, causing a latency spike. The solution would
be to clear reference cycles in one pass, and then actually free the ghost objects (deleting
the final `Gc` which the `Heap` uses to refer to them, and therefore deallocating them) in
a separate pass.

## `"unsafe-internals"` Mode

In `"unsafe-internals"` mode, `Gc` is just a thin wrapper over a raw pointer. This makes it
very fast but also very unsafe - if there is an extant `Gc` for an object which is not
transitively reachable from a `Root`, use-after-free could occur. This invariant needs to be
upheld manually.

In order to mitigate the risk, we don't permit the user to access `Gc`, and we limit our own use 
of `Gc`: at the time of writing, it's only used extensively in `vm.rs`, `collections.rs`, 
`class.rs` and `wrap.rs`. We use `Root` everywhere else, including throughout the stdlib, despite 
the moderate performance cost from doing so.
