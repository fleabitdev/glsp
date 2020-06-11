# Macros

We've now described [how GameLisp's parser converts text into data](syntax-and-types.md), and 
[how GameLisp's evaluator executes data as code](evaluation.md).

To complete the picture, there's one more step that we need to discuss. Just before evaluating any
code, GameLisp's expander performs transformations on the code tree.

It's easy to hook in your own functions to customize the behaviour of the expander. These 
functions, which transform GameLisp data/code into different GameLisp data/code, are known as 
macros.


## The Expansion Algorithm

This is the basic algorithm which GameLisp uses to expand a form in place:

1. If the form is not an array, or if it's an empty array, then no expansion is performed.

2. Expand the array's first element in place.

3. If the array's first element is now a symbol, and that symbol is currently bound to a macro 
   function, then invoke that macro function, passing in all of the array's elements as arguments. 
   **Replace the array with the result of the macro invocation**, then restart from step 1.

4. If the array doesn't start with [`quote`](../std/quote), expand each of its arguments in place, 
   from left to right (starting with the array's second element, then moving on to its third 
   element, and so on.)

In simple terms, this algorithm traverses the entire source tree, looking for arrays which
resemble function calls. When the array's "callee" is a symbol which is bound to a macro,
that macro is called like a function, and then the entire array is replaced with the result of 
the call.
	
	; if the symbol `name` is bound to a macro, the expander will 
	; invoke ((macro 'name) a b c), and then replace the (name a b c) 
	; form with the result of that call.
	(name a b c)

The `(splice)` macro is a special case: it's replaced by each of its argument forms, inserted 
next to one another in-place.
	
	(some-function (splice a) b (splice c d) (splice) e)

	; after macro-expansion...
	(some-function a b c d e)


## Binding Macros

A macro is just a normal GameLisp function, of type `fn` or `rfn`. It takes zero or more
forms as its input, and produces one form as its output.

Like variables, macros can be bound globally or locally. To manipulate a symbol's global macro
binding, use the builtin functions [`bind-macro!`](../std/bind-macro-mut), 
[`del-macro!`](../std/del-macro-mut), [`macro`](../std/macro) and [`macro=`](../std/set-macro).

To introduce a local macro binding, you can use the [`let-macro` special form](../std/let-macro) 
wherever you would use `let`.
	
	; these two forms are similar, but the first form binds its
	; fn to a local variable, and the second form binds its fn
	; to a local macro.

	(let add (fn (a b)
	  (+ a b)))

	(let-macro add (a b)
	  (+ a b))


## Example: `when` and `unless`

Recall the syntax for the `if` special form: `(if condition then else)`.

This syntax is easy to understand when working with very small forms which fit onto a single line,
but when working with complicated multi-line forms, it's not always ideal.
	
	; at a glance, you might mistakenly think that this form will 
	; print both lines when i is less than 100
	(if (< i 100)
	  (prn "first line")
	  (prn "second line"))

	; the do and #n forms here add a lot of visual noise
	(if drawing-suppressed?
	  #n
	  (do
	    (draw-rect x y w h)
	    (draw-circle center-x center-y radius)))

This is exactly the type of problem which can be solved using macros: we have a small bit
of awkward, repetitive or confusing syntax, and we'd like to make it more clear.

The `when` macro is shorthand for "if the condition is true, evaluate a `do` form; otherwise,
evaluate `#n`".
	
	(bind-macro! 'when (fn (condition-form ..do-forms)
	  `(if ~condition-form
	    (do
	      ~..do-forms)
	    #n)))

	(when (< i 100)
	  (prn "first line")
	  (prn "second line"))

The `unless` macro is similar, but it evaluates the `do` form when its condition is false.

	(bind-macro! 'unless (fn (condition-form ..do-forms)
	  `(if ~condition-form
	    #n
	    (do
	      ~..do-forms))))

	(unless drawing-suppressed?
	  (draw-rect x y w h)
	  (draw-circle center-x center-y radius))


## Quasi-quotation

Our `when` and `unless` macro definitions introduced several [unfamiliar pieces of 
syntax](syntax-and-types.md#abbreviations): 

- `` `x `` is an abbreviation for [`(backquote x)`](../std/backquote)
- `~x` is an abbreviation for [`(unquote x)`](../std/unquote)
- `~..x` is an abbreviation for [`(unquote (splay x))`](../std/splay)

Recall that the [`quote` special form](../std/quote), abbreviated `'`, suppresses evaluation. 
If you want to produce a tree of nested arrays, then rather than constructing them 
element-by-element, it's much simpler to just write them down as text and `quote` them so that 
they're returned verbatim.
	
	; these two forms produce equivalent results

	'((one eins) (two zwei) (three drei))
	
	(arr (arr 'one 'eins) (arr 'two 'zwei) (arr 'three 'drei))

`backquote` is like `quote`, but more powerful. 

Firstly, `backquote` always allocates an entirely new, mutable array (whereas `quote` returns a 
shared, immutable array - this is usually more efficient, but it's not always what you want). It's
like a shorthand for the [`(arr ...)` constructor](../std/arr).

Secondly, `backquote` allows evaluated forms and quoted forms to be mixed with one another.
This is called "quasi-quotation", because only part of the form is quoted. If you want some forms
within a backquote to be evaluated while building the array, prefix them with `~`. All other forms 
will be quoted.
	
	(let one-de 'eins)
	(let two-de (fn () 'zwei))

	; the following three forms all produce equivalent results

	'((one eins) (two zwei) (three drei))

	`((one ~one-de) (two ~(two-de)) (three ~(sym "drei")))

	(arr (arr 'one one-de) (arr 'two (two-de)) (arr 'three (sym "drei")))

We briefly mentioned in the previous chapter that prefixing one of `arr`'s arguments with `..`
will cause the full contents of that argument to be "splayed" into the resulting arr. This is
particularly useful when working with `backquote` and `unquote`, as we demonstrated with
`when` and `unless` [above](#example-when-and-unless).


## `macro-no-op`

Within a macro, you can call the built-in function `(macro-no-op)`. This immediately cancels
execution of the macro, and it suppresses step 3 in the [algorithm](#the-expansion-algorithm). 
The current form is left as it is, and the algorithm proceeds to step 4, expanding the form's 
children as normal.

This enables the same name to be simultaneously bound both to a macro, and to a function or special
form. Without `(macro-no-op)`, it would be impossible for a macro to expand to a function call
which shares the same name, because it would trigger an endless loop: `(the-fn a b)` would expand 
to `(the-fn a b)` which would expand to `(the-fn a b)`... but `(macro-no-op)` breaks that cycle.

We've previously mentioned that the `let` special form can accept an arbitrary number of
arguments: with three or more arguments, it behaves like multiple consecutive calls to `let`.
This is actually achieved using a macro which is globally bound to the `let` symbol:
	
	; this form...
	(let a b, c d)

	; ...expands to this one:
	(splice
	  (let a b)
	  (let c d))

	; the `splice` macro can expand into multiple forms, so it becomes:
	(let a b)
	(let c d)

	; each of those (let) forms also invokes the (let) macro, but when that
	; macro is invoked with two or fewer arguments, it just calls (macro-no-op)

More generally, macros and variables occupy entirely different namespaces. A particular symbol
can be bound to a global variable, a macro, both, or neither. A local variable binding will not
shadow a local or global macro, and `let-macro` will not shadow a local or global variable.


## Order of Expansion

There's a small problem with our expansion algorithm: if global macros are defined dynamically 
during evaluation, and if code is expanded before being evaluated, how is it that a global macro 
defined early in a source file can be used to expand code later on in that same source file?
	
	(bind-macro! 'fizz (fn ()
	  `(prn "fizz")))

	; why can we use the 'fizz macro to expand this form?
	(fizz)

It wouldn't be possible for us to interleave expansion with evaluation, because that would be
much too slow: we would need to re-expand every form every single time that it's evaluated. 
This would force GameLisp to be an [AST-walking interpreter], making it many times slower 
than it is today. It's essential that we have a separate expansion step before evaluation.

[AST-walking interpreter]: https://en.wikipedia.org/wiki/Interpreter_(computing)#Abstract_syntax_tree_interpreters

We could set up the expander so that it's capable of detecting `bind-macro!` calls and evaluating
them during expansion, but that would mean that `bind-macro!` is no longer truly dynamic. Patterns
like this would stop working:
	
	(unless file-access-disabled?
	  (bind-macro! 'write-file (fn (file content)
	    `(stream-to-file ~file ~content))))

The solution is a simple compromise. When working their way through the forms in a GameLisp
source file, the expander and evaluator "take turns". The first form is expanded; then the first 
form is evaluated; then the second form is expanded; then the second form is evaluated... and so 
on.

This means that a global macro binding can be used later on in the same source file, but it can't 
be used later on in the same toplevel form. If you want to define a macro to be used locally,
you should use `let-macro` instead.
	
	(do
	  (bind-macro! 'fizz (fn ()
	    `(prn "fizz")))

	  (fizz)) ; this doesn't work...

	(fizz) ; ...but this does

	(do
	  (let-macro fizz ()
	    `(prn "fizz"))

	  (fizz)) ; ...and this does


## Toplevel Scopes

Although evaluation and expansion do only process one toplevel form at a time, that's not the
whole story. If each toplevel form were completely separate from the next, it would be impossible
to use forms like `let` or `let-macro` at the toplevel.

We say that under some circumstances, a group of forms is processed in a "toplevel scope". For
example, when loading a GameLisp source file, the toplevel scope continues until the end of 
that file. This means that a toplevel `let` form works exactly as you would expect.

### Evaluation APIs

The function for running a GameLisp source file is called [`load`](../std/load). It opens a source 
file, reads it into a string, parses that string into a series of forms, and expands and evaluates 
each form in the same toplevel scope.

GameLisp projects are likely to end up with a modular, branching tree of files: the source file 
`main.glsp` calls `(load "engine.glsp")`, which calls `(load "engine-drawing.glsp")`, and so on. 
In order to prevent the same source file from accidentally being loaded twice, we provide the 
function [`require`](../std/require). If the current GameLisp runtime has already received a 
`require` call for the specified file, it silently does nothing. Otherwise, `require` is exactly 
the same as `load`.

[`eval`](../std/eval) and [`eval-multi`](../std/eval-multi) are like `load`, but they receive 
their forms as arguments rather than parsing them from a string. `eval-multi` accepts an array of
 forms, expanding and evaluating those forms one after the other, all in the same toplevel scope. 
`eval` is the same, but its argument is a single form rather than an array of forms.


## Hygiene

If you've worked with Rust's `macro_rules!` macros, you'll know that they're [hygienic]: it's not 
possible for a macro to name a local variable from outside its own lexical scope, unless that 
name was passed in as one of the macro's arguments.

```rust
macro_rules! unhygienic_macro {
	() => (println!("{}", local_name));
};

fn scope() {
	let local_name = 42;

	macro_rules! hygienic_macro {
		() => (println!("{}", local_name));
	};

	hygienic_macro!(); //prints 42
	unhygienic_macro!(); //error: cannot find value `local_name` in this scope
}
```

This is good for program correctness, but it can also be inconvenient. Because GameLisp values 
convenience very highly, GameLisp's macros are unhygienic.

The main risks when working with unhygienic macros are that you may unintentionally refer 
to a local variable when you mean to refer to a global one, or you may "leak" a local variable 
which is supposed to be private to the macro's implementation.
	
	; this macro checks that adding 1 to 2 produces 3 before executing a
	; block of code. defensive programming!
	(bind-macro! 'with-add-assertion (fn (..body)
	  `(do
	    (let tmp 1)
	    (unless (== (+ tmp 2) 3) 
	      (bail "addition is broken!"))
	    ~..body)))

	; prints 1 rather than 5: addition is broken, but we don't detect it 
	; because `bail` is also broken!
	(do
	  (let + -)
	  (let bail (fn (ignored) #n))
	  (with-add-assertion
	  	(let x 3)
	    (prn (+ x 2))))

	; the macro shadows `tmp`, printing 1 rather than the expected 7
	(let tmp 7)
	(with-add-assertion
	  (prn tmp)) 

You can solve the first problem by explicitly accessing the global rather than local version
of a name. (However, this should rarely be necessary - it's unusual for a global name to be
shadowed by a local one, and when it does happen, it's something that the overrider probably
wants to affect *all* nested code, including code generated by macros.)
	
	(bind-macro! 'with-add-assertion (fn (..body)
	  `(do
	    (let tmp 1)
	    (unless (== (+ tmp 2) 3) 
	      ((global 'bail) "addition is broken!"))
	    ~..body)))

The second problem is more insidious and important: macros often need to define temporary 
variables, and needing to come up with a unique name for each of them would be painful. This
is how the C preprocessor ended up with silly names like `__FILE__` and `__STDC_VERSION__`.

We solve this problem using [`gensym`](../std/gensym), a built-in function which returns a unique 
symbol. If you use a fresh `gensym` for each local binding generated by a macro, you don't need 
to worry about accidentally shadowing the user's own variables:
	
	(bind-macro! 'with-add-assertion (fn (..body)
	  (let temp-sym (gensym))

	  `(do
	    (let ~tmp-sym 1)
	    (unless (== (+ ~tmp-sym 2) 3) 
	      (bail "addition is broken!"))
	    ~..body)))

This is such a common pattern that we have special syntax for it: when `backquote` detects
a quoted symbol which ends with `#`, it replaces each occurence of that symbol with the result
of a `(gensym)`.

	(prn `(foo# foo# bar#)) ; prints #<gs:foo:0> #<gs:foo:0> #<gs:bar:1> 

	(bind-macro! 'with-add-assertion (fn (..body)
	  `(do
	    (let tmp# 1)
	    (unless (== (+ tmp# 2) 3) 
	      (bail "addition is broken!"))
	    ~..body)))

[hygienic]: https://en.wikipedia.org/wiki/Hygienic_macro


## Manual Expansion

You'll sometimes want to expand a form without immediately evaluating it. For example, when 
writing a macro, you might want to perform some processing on the expanded version of its
arguments, rather than the unexpanded version.

The built-in function [`expand`](../std/expand) takes a single value as its argument, recursively 
expands it according to the [usual algorithm](#the-expansion-algorithm), and then returns the 
expanded result.

On the other hand, [`expand-1`](../std/expand-1) gives you finer control over the expansion 
process by performing a single "step" of the expansion algorithm at a time. It also allows you 
to override the expansion function, and to detect when [`macro-no-op`](../std/macro-no-op)
is called.
