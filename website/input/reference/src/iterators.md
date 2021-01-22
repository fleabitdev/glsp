# Iterators

Rust's iterators are a joy to use. They often enable you to replace a dozen lines of imperative
code with a single line of declarative code, without obscuring your program's meaning.

This kind of expressiveness seemed like a great fit for GameLisp, so the language comes with an
iterator library which is, for the most part, shamelessly copied from Rust. (To be fair, Rust 
originally copied many of its iterator APIs from Python...)

    (let text "Revered. Exalted. Wise.")

    (for word in (->> (split text \space) (rev) (map uppercase))
      (pr word " ")) ; prints WISE. EXALTED. REVERED. 


## Iteration

The [`iter` primitive type](syntax-and-types.md#type-summary) is like a Rust [`Iterator`],
except that it's dynamically typed and heap‑allocated. (Don't panic! GameLisp is smart enough
to reuse an iterator's heap storage when you're done with it, so the allocation is very cheap.)

[`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html

The simplest way to allocate a new iterator is by calling [`(iter x)`](../std/iter), where `x` is 
some kind of collection or sequence. We say that if something can be passed to the `iter` 
function, it belongs to the [`iterable` abstract type](syntax-and-types.md#abstract-types). 
The following types are iterable:
    
- Arrays iterate over their elements.
- Strings iterate over their characters.
- Tables iterate over their entries as `(key value)` pairs. Each pair is a newly-allocated, 
  two-element array.
    - Alternatively, you can use the [`(keys tbl)`](../std/keys) function to iterate over a 
      table's keys, or the [`(values tbl)`](../std/values) function to iterate over its values.
- Coroutines will be discussed in the [next chapter](coroutines.md).
- Passing an iterator to `(iter x)` is the identity operation - it just returns `x`.

To advance an iterator and return its next item, use the [`iter-next!` 
function](../std/iter-next-mut). If the iterator has no more items, it will return `#n`.

However, you can't assume that an iterator is finished just because it's returned `#n` - if so, 
iteration over the array `(1 2 #n 4 5)` would stop after the first two items! Instead, you should 
use the function [`iter-finished?`](../std/iter-finished-p). If it returns `#t`, then the iterator 
has no more items, and the previous `#n` item should be discarded.

### The `for` Macro

GameLisp comes with a [`for` macro](../std/for), which is very similar to Rust's `for` loop. It 
takes an iterable, and evaluates its body once for each item produced by the iterable, binding 
that item to a [pattern](patterns.md). [`break`](../std/break) and [`continue`](../std/continue) 
work as expected.

    (for element in '(1 2 3 4 5)
      (prn (* element 10)))

    (for (key value) in table
      (ensure (sym? key))
      (prn "{key}: {value}"))

`for` isn't doing anything special - it just invokes the `iter`, `iter-next!` and `iter-finished?`
functions.


## Standard Iterators

GameLisp comes with a large library of built-in iterators. Almost all of Rust's standard
iterators are included: [`enumerate`](../std/enumerate), [`zip`](../std/zip), [`map`](../std/map), 
[`lines`](../std/lines), and so on. You can take a look at [the standard library 
documentation](../std/iterators) for the full list.

Unlike Rust, GameLisp's [`once`](../std/once) and [`repeat`](../std/repeat) iterators can accept 
multiple arguments. If you want an empty iterator which won't produce anything, just call 
`(once)` with no arguments.

[`rn`](../std/rn) counts upwards from one number to another. `(rn 5 10)` is equivalent to the Rust
iterator `5 .. 10`, and `(rn 8)` is equivalent to `0 .. 8`. If you need an inclusive upper bound,
you can use [`rni`](../std/rni): `(rni -5 5)` is equivalent to `-5 ..= 5`.

Because `rn` is such a common iterator, we provide the [`forn` macro](../std/forn) to make it more
convenient to use. (`forn` should be read as a contraction of `for rn`, in the same way that
`defn` is a contraction of `def fn`.)
    
    (forn (digit 0 10)
      (prn digit))

    ; ...is equivalent to...

    (for digit in (rn 0 10)
      (prn digit))

### Double-Ended Iterators

Some iterators are "double-ended": items can be produced both from their back and from their
front. For example, array and string iterators are double-ended. You can query whether an iterator
is double-ended using the [`iter-double-ended?`](../std/iter-double-ended-p) function, and you can 
produce items from the back of a double-ended iterator using 
[`iter-next-back!`](../std/iter-next-back-mut).

[`rev`](../std/rev) takes a double-ended iterable and reverses it, treating its back as its front 
and its front as its back.

### Exact-Size Iterators

Some iterators know more about their length than others do. For example, a `rn` iterator knows
the exact number of items it will return, but a `lines` iterator has no way to predict its item 
count in advance.

We don't provide an equivalent to Rust's [`size_hint()`][0], because it wouldn't be useful. 
GameLisp doesn't provide any way for you to manipulate the capacity of its collections or reserve
memory in advance.

[0]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.size_hint

Instead, the [`len`](../std/len) function can accept an iterator as its argument. If that iterator
knows its exact length, it returns an integer; if it knows itself to be infinite, it returns the
symbol `infinite`; and otherwise it returns the symbol `unknown`.
    
    (prn (len (rn 5))) ; prints 5
    (prn (len (repeat #t))) ; prints infinite
    (prn (len (split text \space))) ; prints unknown

There's nothing to prevent an array or string from being mutated during iteration (although
this is strongly discouraged). This means that array and string iterators do not know their exact 
size. Pushing or popping from the end of a deque during iteration will work as expected, but 
pushing or popping from the start may cause the iterator to behave unpredictably.


## Splaying

We've previously mentioned that you can use [`..`](../std/splay-abbrv), an abbreviation for
[`splay`](../std/splay), to pass all of an array's elements to the array constructor.
    
    (let triad '(x y z))
    (prn (arr 'a 'b 'c ..triad 1 2 3)) ; prints (a b c x y z 1 2 3)

The splay operator is actually much more powerful than this. It will accept *any* iterable, 
and pass all of its items as arguments to *any* function call.

This means that there's no need for GameLisp to have a [`collect`][1] function: you can just splay
an iterator while calling [`arr`](../std/arr), [`str`](../std/str), [`tab`](../std/tab), or any 
other constructor function.

[1]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect
    
    (prn (str ..(take 5 (repeat \A)))) ; prints AAAAA

There's also no need for GameLisp to have [`apply`](http://clhs.lisp.se/Body/f_apply.htm): you 
can just splay an array as a function's last argument instead.

If you want to take the sum of an array of numbers, there's no need to look up the API for `fold`.
Addition is a variadic function, so you can just call `(+ ..the-array)`. The smallest element of
a collection is `(min ..coll)`. To test whether an array of numbers is sorted, call 
`(<= ..numbers)`. Appending the contents of one array onto another is just `(push! arr0 ..arr1)`.


## Indexing

Arrays, strings, objects and classes are normally indexed using an integer or a symbol. However,
it's possible to index them using an iterable instead.

This returns a new iterator which takes each item in turn from the original iterable, indexes
the collection using [`[coll item]`](../std/access), and produces the resulting element.

In effect, `[coll iterable]` is equivalent to [`(map (fn1 [coll _]) iterable)`](../std/map).
    
    ; re-order an array's elements
    (let shuf (arr ..[src-arr '(1 3 0 2)]))
    
    ; equivalent to...
    (let shuf (arr [src-arr 1] [src-arr 3] [src-arr 0] [src-arr 2]))

    ; swizzle an object's fields
    (let offset (Vec3 x y z))
    (let swizzled (Vec3 ..[offset '(y z x)]))

    ; discard every second character in a string
    (let text "You're filled with DETERMINATION.")
    (prn ..[text (step-by 2 (rn (len text)))]) ; prints Yur ildwt EEMNTO.

    ; use multiple object fields as consecutive function arguments
    (draw-sprite spr ..[very-long-coordinates-name '(x y)])

    ; equivalent to...
    (draw-sprite spr [very-long-coordinates-name 'x] [very-long-coordinates-name 'y])

Note that tables do not support this kind of indexing. This is because table keys can belong
to any primitive type, including iterators, arrays, strings, and so on. If you were to call
`[table '(0 0 0)]`, it would be ambiguous whether you were trying to access the key `(0 0 0)`
once, or trying to access the key `0` three times.


## Arrows

Creating a complicated iterator might involve several deeply-nested function calls.

As ever, the [arrow macros](built-in-macros.md#arrows) are the best way to flatten out
a deep call hierarchy. `->>` is often a good choice when working with iterators,
because [iterator adapters](../std/iterators#iterator-adapters) usually expect an iterator or 
iterable as their last argument.
    
    (->> my-array (step-by 3) (map (fn1 (+ _ 10))) enumerate)

    ; ...is equivalent to...

    (enumerate (map (fn1 (+ _ 10)) (step-by 3 my-array)))

The arrow macros include special handling for the splay operator. If you prefix one of the 
arrowed function calls with `..`, then the result of the previous function will be splayed.

    (->> my-array (step-by 3) (map abs) (filter (fn1 (< _ 10))) ..arr)

    ; ... is equivalent to...

    (arr ..(filter (fn1 (< _ 10)) (map abs (step-by 3 my-array))))
