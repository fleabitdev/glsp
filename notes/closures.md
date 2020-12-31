# Closures

Our implementation of closures aims to be simple, rather than having optimal performance.

The basic strategy is that, when a local variable has the potential to be captured by a closure,
we always store it on the heap rather than the stack. We say that it's been "promoted" to the heap.
We call the storage for these variables "Stays", because they enable the variable to "stay
around" indefinitely, unlike normal local variables which are lexically scoped. Allocating a `Stay` 
is cheap, because they're quite small and they can easily be recycled by gc.rs.

When a `(let)` form binds a local variable which may be captured by a `(fn)` form later on, the
encoder doesn't allocate a register for the variable - instead, it emits the `MakeStay`
instruction to allocate a new `Stay`, and any future references to that variable will use
the `LoadStay` and `SetStay` instrs rather than the usual `CopyRegister`.

(Previously, we would unconditionally create a `Stay` for each potentially-captured local variable, 
as soon as its enclosing fn was called. Unfortunately this meant that if a local variable was 
unbound and then rebound, e.g. within a `(for)` loop, each occurence of the variable would share 
the same `Stay`.)

We make an exception for parameter local variables: they are promoted as soon as the function is
called, rather than being promoted by a `MakeStay` instr. Otherwise, we would run into problems
with optional parameters (their `MakeStay` instr wouldn't necessarily be executed).

Note that promotion is *not* delayed until the variable is actually captured. For example, when 
this fn is called, it will allocate an unnecessary `Stay` for the `never-captured` local:
	
	(fn ()
	  (let never-captured)
	  (return 1)
	  (fn () never-captured))

In practice, if a variable *can* be captured it almost certainly *will* be captured, so the 
performance impact there is trivial.

When a `(fn)` form is evaluated, it executes a `MakeGFn` instr. This allocates a new `GFn` which
captures a list of stay-indexes from the current stack frame. These are the same indexes used
by the `LoadStay` and `SetStay` instrs. Note that `MakeGFn` doesn't allocate any new `Stays` - 
it just collects a set of references to `Stays` which already exist.

The `Vm` maintains a stack of `Option<Raw<Stay>>s`, which grows and shrinks alongside the register
stack as fns are called and returned. When a `GFn` is called...
	
- We push a `None` placeholder to the stack for each local variable binding within this fn 
  which will potentially be promoted to a `Stay` using `MakeStay`

- We immediately promote some parameter local variables to the heap, pushing `Some(raw_stay)` 
  for each of them

- We push `Some(raw_stay)` for each captured `Raw<Stay>` from the `GFn` which is being called.

If a local variable is captured by a `(fn)` which is nested within another `(fn)`, then the local
variable will emit `MakeStay` as normal; the outer `(fn)` will capture it as though it's referenced 
in that fn (even though it's not!); and then the inner `(fn)` will capture it from the outer `(fn)`.

Toplevel `(let)` forms are always stored in a `Stay` by the evaluator. Whenever it encodes
some bytecode, it provides a table of toplevel lets. The encoder uses this table to establish the 
initial set of local-variable bindings for that `Bytecode`.
