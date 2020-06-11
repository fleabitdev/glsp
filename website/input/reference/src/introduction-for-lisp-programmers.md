# Introduction for Lisp Programmers

Veteran Lisp programmers will find some parts of GameLisp's design surprising. I've done my best 
to preserve Lisp's strengths, but I haven't hesitated to remove or modify features which are 
standard in other Lisp dialects.

The most Lispy features of GameLisp are its homoiconicity, its Common‑Lisp‑style macro system, and 
the powerful metaprogramming which that implies. The parser, printer, symbols, and data‑as‑code 
should feel familiar to any Lisp programmer.

The *least* Lispy feature of GameLisp is the fact that it's not a functional language. Game code 
tends to be object-oriented and imperative, so GameLisp is an object-oriented, imperative
language. Like Rust, it sprinkles in a little bit of functional flavour here and there (such as 
its Rust-style [iterator adapters](iterators.md) and [pattern-matching](patterns.md)) - but unlike 
Rust, most data is mutable-by-default and there are no restrictions on shared ownership.

Because it's not a functional language, GameLisp doesn't use lists. The singly-linked-list 
data structure loses many of its advantages when coding in a non-functional style, while still having
many disadvantages (`caddar`, poor big-O complexity for many operations, extra pressure on the 
allocator...), so I've completely removed cons cells from the language. Instead, `(a b c)` 
represents a double-ended contiguous array 
(specifically, a [`VecDeque`](https://doc.rust-lang.org/std/collections/struct.VecDeque.html)). 
I've found that for representing syntax, and as a general-purpose sequential data structure, 
double-ended arrays are significantly more versatile and convenient than lists. The slightly 
increased cost of `(rest x)` hasn't been a problem in practice.

(I'm aware that Lisp is an acronym for *Lis*t *P*rocessing, and I do appreciate the irony...)

Finally, I've made GameLisp's syntax and keywords as "Rust-like" as possible, to minimize the
mental friction when switching between the Rust and GameLisp parts of your codebase. I've also 
changed a handful of names to make them more concise or more descriptive. Notable changes:
	
- `lambda` has been renamed to `fn`.

- `set!`/`setf` has been renamed to `=`.
	- The numeric-equality test `=` has been renamed to `==`.
	- The naming convention for assignment, `set-something!`, is now `something=`.
	- I've retained the `!` suffix for other mutating functions, like `push!` and `inc!`, and the 
	  `?` suffix for predicates, like `int?`.

- `progn`/`begin` has been renamed to `do`.

- The special values for true, false and nil are written as `#t`, `#f` and `#n`. Nil, false, and
  the empty sequence `()` are all distinct from one another.

- `let` no longer defines a block: it introduces a new binding which is scoped to the end of its 
  enclosing block, similar to `define` in Scheme or `let` in Rust.

- Conditionals follow the conventions from 
  [Racket](https://docs.racket-lang.org/style/Choosing_the_Right_Construct.html): prefer `cond`
  or `match` rather than `if`, unless it's a very brief conditional which fits into a single line.

- Sequence-processing primitives have been renamed to match their Rust equivalents: `len` 
  rather than `length`, `rev` rather than `reverse`, and so on.

- A small number of new abbreviations (all of which have a [straightforward 
  representation](syntax-and-types.md#abbreviations)): 
	- `[a b]` to access the field `b` in the collection `a`.
	- `@name` to access the field `name` on a method's self-object.
	- `(.meth-name a b c)` to invoke the method `meth-name` on the object `a`.
	- `(callee ..args x)` to splay the contents of the `args` collection into individual 
	  function arguments. This is similar to `(apply)`, but more powerful and versatile.

- The quasiquote syntax resembles Clojure. Forms are unquoted with `~` rather than `,`, and symbols 
  can be auto-gensymed with a `#` suffix, as in `name#`. We parse commas as whitespace so that 
  function or macro arguments can be visually grouped together, like `(let a b, c d)`.

Early on in GameLisp's development, its syntax and keywords were much more Scheme-like. These
changes aren't arbitrary - each of them came from battle-testing GameLisp while developing The 
Castle on Fire. They each represent something which has caused me enough frustration that I 
thought it would be worth refactoring a dense, 15,000-line codebase to change it.

That being said, GameLisp is still in its infancy. If you're interested in shaping the growth of
a new Lisp - specifically, if you've spent a little time working with GameLisp and you have a 
suggestion for its syntax, naming conventions or style conventions - please feel free to
[get in touch on GitHub][0]. Some changes which are possible now might be impossible in twelve 
months' time, so don't hesitate!

[0]: https://github.com/fleabitdev/glsp/issues/
