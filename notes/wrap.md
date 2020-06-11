# Wrapper Methods

Wildcard wrappers are straightforward. When we're compiling a class, we just collect a list of
all wildcard wrappers for each unqualified name (in no particular order), and store them in the 
`Class` as a stack. The wrapper method receives a hidden `next-index` parameter, and replaces 
`(@base)` with `(call-base-raw self next-index)`. This function searches up the stack starting 
from the given index, and invokes the first method in the stack which belongs to an enabled state, 
or performs a silent no-op if it reaches end-of-stack for the current unqualified name.

Explicit wrappers are much less straightforward. They essentially form a graph of arbitrary 
references from one explicit `wrap` to any explicit `wrap` or `meth` in any other state or mixin. 
In order for them to make sense, we need to guarantee that, at any given moment, this graph is a 
singly-linked list: there is an outermost explicit `wrap`, which chains through a series of inner 
wrappers, eventually calling a `meth`, without skipping any enabled explicit `wraps` or `meths`. 

We particularly need to support code like this, which has the potential to dynamically become 
invalid if the wrong set of states are enabled simultaneously:
	
	(class A
	  (meth x ())
	  (state B
	    (wrap A:x ()))
	  (state C
	    (wrap A:x ())))

	(let a (A))
	(enab! a 'B)
	(enab! a 'C)
	(.x a) ; which is called, B:x or C:x?

This means that we can't statically prove whether or not a class is valid when we compile it...
static analysis would suggest that the above class is invalid, but preventing it from compiling
would be unnecessarily strict, because `B` and `A` may never be simultaneously enabled.

We can't perform any kind of actual graph analysis every time we enable or disable a state, because 
it would be much too costly. 

Instead, we guarantee that these simple invariants are constantly upheld:

1. There must be no more than one active `meth` for a particular unqualified name.
2. Each active explicit `wrap`'s target must exist and must be active.
3. There cannot be multiple active explicit `wraps` which refer to the same target.
4. There may not be multiple methods bound to the same qualified name (note that wildcard wrappers
   are bound to an unqualified name rather than a qualified name)
5. In the method stack, when A is wrapped by B, A must be stored at a higher index than B.
6. In the method stack, the chain of `(@base)` calls from any explicit `wrap` must eventually 
   reach a `meth` - cycles are forbidden.

We uphold invariants 1 through 3 dynamically, and we guarantee invariants 4 through 6 while 
compiling the `Class`. Together, these invariants are sufficient to reduce the graph to a simple 
sparse vector, which can be traversed using the same (very fast!) approach we use for wildcard 
wrappers.

We uphold invariants 1 through 3 by giving each state three bitmasks, where each bit corresponds 
to another state. When the state is about to be enabled or disabled, we check those bitmasks
against the current set of enabled states:

- The `requires` bitmask indicates which states must be enabled when this state is being enabled.
  It contains all of the states which contain a method which is explicitly wrapped by this state.
  The `Main` state doesn't require itself, even when mixins would imply otherwise.
- The `required_by` bitmask indicates which states must not be enabled when this state is
  being disabled: the inverse of `requires`.
- The `excludes` bitmask indicates which states must not be enabled when this state is being
  enabled. It contains all of the states which wrap a qualified name which is also wrapped by this
  state. It also contains any state which contains a `meth` whose unqualified name collides with 
  one of this state's `meths`.
