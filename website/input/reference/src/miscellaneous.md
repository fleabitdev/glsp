# Miscellaneous

## Freezing Data

GameLisp's variables and collections are mutable by default. This is convenient, but it can also
be nerve-wracking. It's sometimes difficult to know whether or not you have exclusive ownership of
a collection, so there's always the risk that typing `(push! ar x)` or `(clear! coll)` could 
have unintended side-effects in some distant part of your codebase.

The [`freeze!` function](../std/freeze-mut) takes any number of GameLisp values as arguments. 
For each value which refers to an array, string, table, or object, a flag is set on that 
collection which prevents it from being mutated. Future calls to `(= [ob 'x] 10)`, 
`(swap-remove! ar 5)` and so on will trigger an error. Frozen collections can never be unfrozen, 
and they can't even be mutated using the Rust API.

`freeze!` is not recursive: you can always mutate a non-frozen collection, even if you're 
accessing it via a frozen collection. To freeze a value *and* all of the values which it
transitively refers to, use [`deep-freeze!`](../std/deep-freeze-mut) instead.

Finally, you'll sometimes want the security of knowing that a global variable can't be 
accidentally mutated. You can enforce this by calling [`freeze-global!`](../std/freeze-global-mut).
(GameLisp automatically freezes a few of the built-in functions, such as `+`, so that they can be 
optimized more aggressively.)

    (def ar (arr (arr 'a 0) (arr 'b 1)))

    (push! ar (arr 'c 2))
    (prn ar) ; prints ((a 0) (b 1) (c 2))

    (freeze-global! 'ar)
    ;(= ar '()) ; this would be an error

    (freeze! ar)
    ;(push! ar (arr 'd 3)) ; this would be an error
    (push! [ar 0] 0)
    (prn ar) ; prints ((a 0 0) (b 1) (c 2))

    (deep-freeze! ar)
    ;(push! [ar 1] 1) ; this would be an error

### Literals

Using `quote` or self-evaluating types, it's possible to create multiple aliasing references to 
data which was originally passed in to the [evaluator](evaluation.md). For example:
    
    (loop
      (let ar ())
      (push! ar 1)
      (prn ar))

Intuitively, you might expect `ar` to be a fresh, empty array each time it's initialized, so that
the `prn` call prints `(1)` every time. Unfortunately, because empty arrays are self-evaluating,
the `()` form will repeatedly return the same array. This loop would print `(1)`, then `(1 1)`, 
then `(1 1 1)`...

In order to prevent this, whenever GameLisp encounters a `quoted` or self-evaluating array, string
or table, if it's not already deep-frozen then it will be replaced with a 
[deep-cloned](#cloning-data), [deep-frozen](#freezing-data) copy of itself. This means that 
self-evaluating forms and `quoted` forms are always immutable, avoiding problems like the above.

To make unintended mutation even less likely, all data produced by the parser is automatically 
frozen. You can [clone](#cloning-data) the data if you need a mutable copy.



## Cloning Data

The [`clone` function](../std/clone) receives a single value as its argument. If this value is 
an array, string, table, or iterator, it returns a shallow copy of that value. The copy will be 
mutable, even if the original collection was frozen.

The [`deep-clone` function](../std/deep-clone) shallow-clones its argument, and then recurses 
through to clone any collections referred to by the argument, and any collections referred to 
by *those* collections, and so on.

Cloning an iterator will never clone the array, string or table which is being iterated, even when 
using `deep-clone`. When they encounter an iterator, both `clone` and `deep-clone` perform just 
enough copying to ensure that [`iter-next!`](../std/iter-next-mut) will not modify the original 
iterator.


## Equality

GameLisp provides four different equality tests. This is a necessary complication: in 
languages like Rust, the `==` operator is heavily overloaded, which it its own [source][1] 
of [complexity][2].

[1]: https://doc.rust-lang.org/std/cmp/trait.PartialEq.html
[2]: https://doc.rust-lang.org/std/rc/struct.Rc.html#method.ptr_eq

In GameLisp, the [`==` function](../std/num-eq) specifically tests for numeric equality. It 
exists to complement the other numeric comparison functions: `<`, `<=`, `>=` and `>`. It's an 
error to pass a non-numeric value to `==`.

The [`same?` function](../std/same-p) tests for [identity]. For all [reference 
types](syntax-and-types.md#type-summary), two references are the `same?` if they point to the 
same allocation.

[identity]: https://en.wikipedia.org/wiki/Identity_(object-oriented_programming)

`same?` is usually more strict than you'd like. For example, `(same? '(1 2) (arr 1 2))` will 
return `#f`, because it sees its arguments as being two distinct arrays, even though they have 
identical contents.

You'll generally want to use the [`eq?` function](../std/eq-p) instead. It differs from `same?` 
in that arrays, strings and tables are deeply inspected: they compare equal when their contents
are recursively identical.

The final built-in equality test is `keys-eqv?`, which was 
[discussed previously](tables.md#key-equivalence).

### Multiple Comparisons

Functions like `==` and `eq?` are variadic. `(== a b c d)` tests whether `a`, `b`, `c` and `d` 
are all `==` to one another.

If you need to test one value against multiple others, you can use the functions 
[`==any?`](../std/num-eq-any-p), [`same-any?`](../std/same-any-p) and 
[`eq-any?`](../std/eq-any-p).

    (ensure (eq-any? (coro-state c) 'newborn 'running 'paused 'finished 'poisoned))

    (when (same-any? current-room graveyard dungeon prison-cell)
      (inc! fear-level))


## Time and Date

Because the Rust standard library only has limited facilities for handling time, the same is
true for GameLisp.

The [`time` function](../std/time) returns a high-precision [monotonic timestamp] measured in 
seconds, and the [`sleep` function](../std/sleep) suspends the current thread for a specified 
number of seconds. Note that although `time` should have excellent precision on all platforms, 
`sleep` is often very imprecise, particularly on Windows. You should prefer to time your main 
loop using an external signal, such as blocking on VSync.

[monotonic timestamp]: https://doc.rust-lang.org/std/time/struct.Instant.html

The only date-and-time facility provided by GameLisp is the [`unix-time` 
function](../std/unix-time), which returns the number of elapsed whole seconds in the 
[UNIX epoch]. In order to avoid the [2038 problem] it returns a string, such as `"1153689688"`. 
It's intended to be used as a basic timestamp for logging - it can't be readily converted into a 
human-readable format.

[UNIX epoch]: https://en.wikipedia.org/wiki/Unix_time
[2038 problem]: https://en.wikipedia.org/wiki/Year_2038_problem


## Last But `(not least)`...

The [`not` function](../std/not) returns `#t` for a [falsy](syntax-and-types.md#bool) argument, 
and `#f` for a [truthy](syntax-and-types.md#bool) argument.

The [`identity` function](../std/identity) accepts a single argument and returns it unchanged.
The [`no-op` function](../std/no-op) accepts any number of arguments and returns `#n`. Both are
occasionally useful as first-class functions.

The [`fn0`](../std/fn0) and [`fn1`](../std/fn1) macros provide a concise way to define functions
with zero or one arguments. `fn1` can use a single underscore, `_`, to refer to its argument. 
Within a `fn1` form, you should try to avoid discarding a value in a pattern (e.g. 
`(match ... (_ x))`), since it can be visually confusing.

    (ensure (all? (fn1 (eq? _ 'red)) colors) "non-red color detected")

    ; ...is equivalent to...

    (ensure (all? (fn (color) (eq? color 'red)) colors) "non-red color detected")

In Section 2, we'll discuss how to invoke and tune the [garbage collector](garbage-collection.md) 
from Rust. You might find that you prefer to do that from within GameLisp, in which case you
can use the functions [`gc`](../std/gc) and [`gc-value`](../std/gc-value).


## Input/Output

You may have noted the conspicuous absence of an input/output (IO) library. GameLisp doesn't
provide any built-in functions for filesystem access, beyond `load`, `require` and `include`.

This is a deliberate design choice. IO is a huge topic: a full-featured IO library
for game development would need to include byte streams, compression, string streams, stdio,
text encodings, networking, non-blocking io, sandboxing, logging... the list goes on.

Meanwhile, your game engine will almost certainly have its own opinions about how filesystem and 
network access should be done. Something as simple as storing your game's assets in a .zip
file might make the entire IO library unusable!

Instead, you're encouraged to use the [Rust API](the-rust-api.md) to bind your engine's existing 
APIs to GameLisp.
