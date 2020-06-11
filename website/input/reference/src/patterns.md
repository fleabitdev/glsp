# Patterns

Checking whether a value has a particular shape, and extracting other values from its interior, 
is a fundamental operation. In languages which don't have first-class support for destructuring, 
this operation tends to be unnecessarily difficult. For example, binding array elements to local 
variables in C is more verbose than it ought to be:

```c
assert(array_len >= 3);
float x = array_ptr[0];
float y = array_ptr[1];
float z = array_ptr[2];
```

Likewise for iterating over an array of 3D points in Lua:

```lua
for _, point in ipairs(points) do
    local x, y, z = point.x, point.y, point.z
    -- ...
end
```

GameLisp includes destructuring facilities which are as powerful as Rust's, with a number of 
changes and additions to reflect the fact that GameLisp is dynamically-typed rather than 
statically-typed.


## Basic Patterns

A pattern may appear anywhere that you might create a new variable binding: the left-hand-side
of a [`let`](../std/let), [`def`](../std/def), [`field`](../std/field), or 
[`with-global`](../std/with-global) form, a function parameter, or the item in a 
[`for` loop](../std/for).

A pattern is a form, but it's not evaluated or macro-expanded in the normal way. Patterns have
their own special evaluation rules.

When a pattern is a symbol, it matches any value and binds it to that symbol.

	(let x-doubled (* x 2))

The symbol `_` is special: it matches any value and discards it.

	(forn (_ 100)
	  (do-something-one-hundred-times))

	(let _ 100)
	(prn _) ; an error: the symbol _ is not bound to a variable

Self-evaluating forms, like strings, quoted forms, tables, empty arrays, and numbers, only match a 
value which is `eq?` to the pattern. The value is then discarded.
	
	; both of these succeed, but without creating any bindings
	(let 10 (+ 5 5)) 
	(let #((a b)) (tab ('a 'b)))

Unlike Rust, patterns in `let` bindings are not required to be infallible. Any pattern can fail
to match. If a mismatch occurs in `let`, `def`, `field`, `with-global` or `for`, it's an error.

	(let "xyz" (arr 'x 'y 'z)) ; an error
	(let 'a-symbol (tab ('a 'b))) ; an error

You can deal with fallible bindings using the [`match`](../std/match), 
[`when-let`](../std/when-let) and [`matches?`](matches-p) macros. 

- `match` checks a value against a series of patterns and executes some code for the first pattern 
  which successfully matches the value, returning `#n` if no patterns match. 
- `when-let` does the same for a single pattern.
- `matches?` returns `#t` when a value matches a pattern, or `#f` when it's a mismatch.

<span></span>
	
	(match (len ar)
	  (0
	    "empty array")
	  (1
	    "array with a single element")
	  (n
	    "array with {n} elements"))

A pattern isn't always just a single form; it's sometimes described by a series of consecutive
forms. For example, wherever you would write a pattern, you can instead write `x at pat`. This
processes `pat` as normal, but also binds the additional variable `x`, which is set to the
pattern's input value. (This is similar to Rust's [`x @ pat` syntax][0].)

[0]: https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html#-bindings


## Predicates

A "predicate" is a function which asks a question. For example, the functions 
[`int?`](../std/int-p), [`>=`](../std/gte), [`eq?`](../std/eq-p) and 
[`fn-yields?`](../std/fn-yields-p) are all predicates.

Any pattern can test an arbitrary predicate using the syntax `pat : pred?`. When `pat` matches
the input value, the predicate function `pred?` is invoked - if the result is false, then 
the match becomes a mismatch.

Note that the `:` is whitespace-sensitive, so `pat: pred?` would be invalid syntax.

	(match arg
	  (i : int?
	    "the integer {i}")
	  (f : flo?
	    "the floating-point number {f}")
	  (val
	    (bail "expected a number but received {(type-of val)}")))

	(let result : num? (calculate-number))

	(when-let f : flo? result
	  (prn "calculated the floating-point number {f}")
	  (prn (matches? result _ : int?))) ; prints #f

When `pred?` is a symbol, the predicate is invoked on the input value being tested, 
`(pred? input)`. Under those circumstances, to avoid confusion, `pat` must be a symbol or `_`.

When `pred?` is not a symbol, it's evaluated as an arbitrary form which has access to all of
the pattern's bindings. If the evaluation result is false, the pattern is a mismatch. The 
form will usually be a function call which inspects one or more of the pattern's variables.
	
	; a predicate can be used like a match guard in Rust
	(match (calculate-x)
	  (x : (== x 0)
	    "zero")
	  (x : (> x 0)
	    "the positive number {x}")
	  (x
	    "the negative number {x}"))

You can test arbitrarily complex predicates by defining your own functions.

	(let-fn u8? (n)
	  (and (int? n) (<= 0 n 255)))

	(defn rgb (r : u8?, g : u8?, b : u8?)
	  (bitor (bitshl r 16) (bitshl g 8) b))

	(ensure (== (rgb 0xb7 0x41 0x0e) 0xb7410e))

Predicates can perform Boolean operations using the forms `(and pred? ...)`, `(or pred? ...)` and 
`(not pred?)`. These forms can be arbitrarily nested inside one another.

	(match f
	  (f : (and flo? (or inf? nan?))
	    "a weird float {f}")
	  (f : flo?
	    "a normal float {f}")
	  (_
	    "not a float"))

	(when-let sym : (and sym? has-global?) input
	  (let g (global sym))
	  (prn "the input is a symbol bound to the global {g}"))

Predicates are not a static type system! When you place a predicate on a variable's definition, it 
only checks the variable's initial value - if the variable is mutated, the predicate may no longer
hold.
	
	(def name : str? "global string")
	(= name 'assigned-symbol)
	(prn (str? name)) ; prints #f


## Arrays

An array pattern will match an array value with exactly the same number of arguments.
	
	(let (one two three) '(1 2 3))
	(let (one two) '(1 2 3 4 5)) ; an error

Each element of the array pattern is, itself, a pattern. 
	
	(let ((a b) (c d)) '((1 2) (3 4)))

Complex patterns should be separated using [commas](syntax-and-types.md#whitespace) to 
improve readability.

	(defn constant-fold (form)
	  (match form
	    ((callee : (has? foldable-ops callee), a : num?, b : num?)
	      ((global callee) a b))
	    (form
	      form)))

	(prn (constant-fold '(+ 2 3))) ; prints 5
	(prn (constant-fold '(+ var-name 3))) ; prints (+ var-name 3)

Predicates can access variables defined for earlier array elements - not just their current
element.
	
	(let (one, two : (> two one)) '(1 2))

In forms like `let` and `def`, when there is no initializer value, the pattern will automatically
succeed and set all of its bindings to `#n`. This is a convenient way to declare several 
uninitialized local variables at once.
	
	(let (width height depth))
	(prn width height depth) ; prints #n #n #n

### Optional Patterns

`(? pat)` is an "optional pattern" - it represents an element which may or may not exist. In an
array pattern, if that element is missing (because the array is too short to have an element at 
that index), the pattern unconditionally succeeds and all of its bindings are initialized to 
`#n`.
	
	(let (a (? b) (? c)) '(1 2))
	(prn a b c) ; prints 1 2 #n

	; the predicate succeeds, even though (int? #n) is false
	(let (d : int?, (? e) : int?) '(4)) 
	(prn d e) ; prints 4 #n

An optional array element may specify a default value, which is used in place of `#n` when the
element is missing. The syntax is `(? pat init)`. When the pattern has a predicate,
`(? pat init) : pred?`, the default value *is* tested by the predicate.
	
	(let (x y (? z 0.0)) '(5.0 3.5))
	(prn x y z) ; prints 5.0 3.5 0.0

While a default value is being evaluated, any bindings from earlier array elements will be in 
scope and initialized.
	
	(let (small (? big (* small 10))) '(5))
	(prn small big) ; prints 5 50

### Rest Patterns

`..pat` is a "rest pattern" - it can be used to capture zero or more contiguous array elements 
which were not captured by any other patterns. Each element is tested against `pat` (including 
calling `pat`'s predicate, if it has one, for each element individually). The rest pattern only 
matches when all of the individual elements match. Each variable binding in `pat` is bound to a 
newly-allocated array which contains one value for each element.
	
	(let pairs '((a 1) (b 2) (c 3)))

	(let (..(letters : sym?, numbers : int?)) pairs)

	(prn letters numbers) ; prints (a b c) (1 2 3)

	; the most common use of rest patterns is to unconditionally collect
	; part of an array into a variable.
	(let (first, second, ..rest : int?) '(0 1 2 3 4))
	(prn first second rest) ; prints 0 1 (2 3 4)

	; the .._ pattern can be used to discard any number of array elements.
	; in that case, no allocation will be performed.
	(let (first .._ last) '(0 1 2 3 4))
	(prn first last) ; prints 0 4

The order of optional and rest patterns is quite flexible. Any of the following would be valid 
array patterns:
	
	(a (? b) (? c))
	(a (? b) ..c)
	(..a b c)
	(a ..b c)

### Functions

Function parameter lists are just array patterns, where the input array contains all of the
arguments to the function call. This means that functions and macros can take advantage of the 
full power of predicates, optional patterns, and rest patterns.
	
	(defn draw-text (text : str?, x : num?, y : num?, 
	                 (? font simple-font) : (is? font 'Font))
	  ...)

	(defn process (first ..rest)
	  ...

	  (unless (empty? rest)
	    (process ..rest)))

Array patterns can be used to emulate function overloading. For example, this function expects
a different parameter list depending on whether its first argument is a deque or a table:
	
	(defn search (..args)
	  (match args

	    ((haystack : deque?, needle)
	      (any? (fn1 (eq? _ needle)) haystack))

	    ((haystack : tab?, key, value)
	      (eq? [haystack key] value))

	    (args (bail "invalid argument list {args}"))))

GameLisp can optimize functions more aggressively when their parameter list follows all of 
these rules:
- `..rest` is either the last parameter, or absent.
- For `(? opt)` and `..rest` parameters, the pattern itself is `_` or a symbol.
- For `(? opt init)` parameters, `init` is either self-evaluating (like `()`) or 
  quoted (like `'(6 7)`).


## `or` Patterns

`or` isn't only useful in predicates. You can use an `(or pat ...)` form wherever you would use 
any other pattern. It matches, and binds, the first matching child pattern. If none of the child 
patterns match, the `or` pattern is a mismatch.

You can think of `or` as being like [`|` in Rust patterns][1].

[1]: https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html#multiple-patterns

	; match on a valid (+), (-), (*) or (/) form. (/) requires at least
	; one argument, but the others can be called with zero arguments.
	(match form
	  ((or ((or '+ '- '*) ..rest)
	       ('/ first ..rest))
	    ...))

All possible variable bindings from all of the child forms will be bound by the `or` pattern.
If a variable isn't present in the sub-pattern which matches, that variable will still be bound, 
but it will be initialized to `#n`.
	
	; search for an item in a deque, which can be defined either by a predicate
	; callback, or by an argument which should be (eq?) to the item.
	(defn search (haystack : deque?, (or is-needle? : callable?, needle-item))
	  (for (i item) in (enumerate haystack)
	    (cond
	      ((nil? is-needle?)
	        (when (eq? needle-item item)
	          (return i)))
	      (else
	        (when (is-needle? item)
	          (return i)))))
	  #n)


## Indexing

When a pattern is an [`access` form](../std/access) - in other words, when a pattern is enclosed 
in square brackets - it will attempt to index its value. The pattern matches if all of the indexes 
are present.

The `access` form's children are usually symbol patterns, in which case each indexed value is 
bound to the corresponding name.
	
	(let [x y z] 3d-vector)
	(let length (cbrt (+ (* x x) (* y y) (* z z))))

	(let [height : int?, weight : flo?] physical-properties)

Each child form may also be an array `(index pat)`, where `index` is a literal value (not 
necessarily a symbol!) used to index the collection, and `pat` is a pattern which is matched 
against the resulting value.

	(let [(x src-x) (y src-y)] src-2d-vector)
	(let [(x dst-x) (y dst-y)] dst-2d-vector)
	(prn "from ({src-x}, {src-y}) to ({dst-x}, {dst-y})")

	(let [(0 zeroth : flo?) (500 five-hundredth : flo?)] huge-array)

`?` and `..` patterns work in much the same way that they do for arrays. The `..` pattern
will collect all remaining key/value pairs into a table; it only matches input collections which
are, themselves, tables. Any predicates are tested against the value, rather than the key.
	
	(let table #((a 1) (b 2) (d 4) (e 5)))

	(let [a (? b) (? c) ..rest : int?] table)
	(prn a b c) ; prints 1 2 #n
	(prn rest) ; prints #((d 4) (e 5)), not necessarily in that order


## File Formats

If you have some custom data files which your game should store - for example, the file format used
by your level editor - then you should consider storing them in the GameLisp text format.

GameLisp already has powerful facilities for parsing and manipulating its own data-types, but 
patterns take things to the next level. For example, a function to validate the level file-format 
for a simple role-playing game might look like this:

	(defn level-valid? (lvl)
	  (matches? lvl [
	    name : str?
	    background-music : sym?
	    (tiles (..(x : int?, y : int?)))
	    (entities (..entity : entity-spec-valid?))
	  ]))

	(defn entity-spec-valid? (entity)
	  (matches? entity [
	    class-name : sym?
	    (coords (x : int?, y : int?))
	    (? init-args ()) : arr?
	  ]))

	; the (level-valid?) function would match a level file with these contents
	#(
		(name "tower-exterior")
		(background-music aria)
		(tiles ((0 5) (3 1) (1 1) (1 1) #|and so on|#))
		(entities (
			#((class-name TowerGuard) (coords (70 95)) (init-args (alert)))
			#((class-name Portcullis) (coords (70 60)))
		))
	)

	; contrast similar code written in a more imperative style...
	(defn entity-spec-valid? (entity)
	  (and
	    (sym? [entity (? 'class-name)])
	    (has? entity 'coords)
	    (do
	      (let coords [entity 'coords])
	      (and
	        (arr? coords)
	        (== (len coords) 2)
	        (do
	          (let x [coords 0], y [coords 1])
	          (and (int? x) (int? y)))))
	    (or
	      (not (has? entity 'init-args))
	      (arr? [entity 'init-args]))))

	(defn level-valid? (lvl)
	  (and
	    (str? [lvl (? 'name)])
	    (sym? [lvl (? 'background-music)])
	    (has? lvl 'tiles)
	    (block tiles-checker
	      (for tile in [lvl 'tiles]
	        (unless (and (arr? tile) (== (len tile) 2) (int? [tile 0]) (int? [tile 1]))
	          (finish-block tiles-checker #f)))
	      #t)
	    (has? lvl 'entities)
	    (all? entity-spec-valid? [lvl 'entities])))

Patterns are concise and powerful, but they should be used cautiously - it can be difficult to 
fully understand the behaviour of a complex pattern. If you find that a pattern you've written
isn't self-explanatory at a glance, consider refactoring it into multiple simpler patterns,
even if that makes your code more verbose.


## Cheat Sheet

|Pattern|Meaning|
|-------|-------|
|`_`|Match, and discard, any value|
|`"hello"`|Match, and discard, the string `"hello"`|
|`'world`|Match, and discard, the symbol `world`|
|`x`|Match any value and bind it to `x`|
|`x : pred?`|Match a value for which `(pred? x)` is true, and bind it to `x`|
|`pat : (form y z)`|As for `pat`, but only matches when `(form y z)` is true|
|`x at pat`|As for `pat`, but additionally binds the input value to `x`|
|`x at pat : pred?`|Combines the previous two cases|
|`(or pat1 pat2)`|Match either `pat1` or `pat2`|
|`(pat1 pat2)`|Match an array of two elements|
|`(pat1 (? pat2))`|Match an array of one or two elements|
|`(pat1 ..pat2)`|Match an array of one or more elements|
|`[x]`|Match a collection with the field `'x`, binding it to `x`|
|`[(x pat)]`|As for `[x]`, but the value is matched against `pat`|
