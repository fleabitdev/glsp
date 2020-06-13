# Built-in Macros

In this chapter, we'll explore a few of the macros built in to GameLisp's standard library.

While reading through, bear in mind that none of these macros are doing anything which requires
special support from the runtime - if they didn't exist, it would be possible for you to implement
all of them yourself, using the information you learned in the [previous chapter](macros.md).


## Control Flow

The [`when` and `unless` macros](macros.md#example-when-and-unless) were introduced previously.

### `while`, `loop`, `until`

[`while`](../std/while) and [`loop`](../std/loop) are similar to their Rust counterparts:

	(let i 0)
	(while (< i 5)
	  (inc! i)
	  (pr i " ")) ; prints 1 2 3 4 

	(loop
	  (prn "around and around we go"))

[`until`](../std/until) is like `while`, but it terminates the loop when its condition is 
["truthy"](syntax-and-types.md#bool), rather than stopping when it's 
["falsy"](syntax-and-types.md#bool). We could rewrite the above `while` loop as:
	
	(let i 0)
	(until (>= i 5)
	  (inc! i)
	  (pr i " ")) ; prints 1 2 3 4 

The [`break`](../std/break) and [`continue`](../std/continue) macros work as they do in Rust. 
By default, all looping constructs return `#n`, but break-with-value is supported:
	
	(prn (loop
	  (break 10))) ; prints 10

### `and`, `or`

The [`and`](../std/and) and [`or`](../std/or) macros provide lazy boolean evaluation, just like 
Rust's `&&` and `||` operators. 

`and` evaluates each of its arguments from left to right. If one of them returns a
["falsy"](syntax-and-types.md#bool) value, the `and` form returns that value and does not 
evaluate any more arguments. Otherwise, it returns the result of evaluating its rightmost 
argument.

Similarly, `or` evaluates each of its arguments from left to right, stopping as soon as it 
produces a ["truthy"](syntax-and-types.md#bool) value.

### `cond`

The [`cond`](../std/cond) macro is reminiscent of Rust's `match`, but it's simpler. (GameLisp does
also have a full&nbsp;fledged `match` macro, which we'll discuss in a [future 
chapter](patterns.md).)

`cond` receives a series of clauses, where the first form in each clause is a 
condition. It checks each condition in turn, evaluating the body of the first clause whose
condition is ["truthy"](syntax-and-types.md#bool), and returning the evaluation result of the
last form in that body.
	
	(cond
	  (condition-0   ; if this condition is truthy...
	    then-0)        ; this form is evaluated and returned
	  (condition-1   ; otherwise, if this condition is truthy...
	    then-1a        ; this form is evaluated, then...
	    then-1b)       ; this form is evaluated and returned
	  (condition-2)  ; otherwise, if this condition is truthy, it's returned
	  (else          ; otherwise...
	    then-e))       ; this form is evaluated and returned

The final clause's condition can be the symbol `else`, in which case the condition always
passes. If there is no `else` clause and none of the conditions are 
["truthy"](syntax-and-types.md#bool), the `cond` form returns `#n`.

If a clause only contains a condition, without any other forms, then that clause's return
value is the result of evaluating the condition itself.

When a branching expression is long enough that it needs to be split across multiple lines,
and it has both a "then" branch and an "else" branch, it's good practice to use `cond` 
rather than `if`:
	
	; can be confusing
	(if (eq? emotion 'angry)
	  (draw 0 0 w h 'crimson)
	  (draw 3 3 (- w 3) (- h 3) 'cerulean))

	; more verbose, but easier to edit and less confusing
	(cond
	  ((eq? emotion 'angry)
	    (draw 0 0 w h 'crimson))
	  (else
	    (draw 3 3 (- w 3) (- h 3) 'cerulean)))


## Defining Globals

We introduced `bind-global!` in the [Evaluation](evaluation.md#global-variables) chapter.
`bind-global!` is such a common operation that we provide a macro shorthand for it, 
[`def`](../std/def).
	
	(def clip:door-open (load-clip "door-open.mp3"))

	; ...is equivalent to...

	(bind-global! 'clip:door-open (load-clip "door-open.mp3"))

Bear in mind that `bind-global!` will trigger an error if its global is already bound, so you
can't necessarily use `def` where you would use `let`. It's generally only used at the toplevel.

`def`'s syntax is otherwise identical to `let`: it can accept three or more arguments
(equivalent to multiple consecutive `def` forms), and it performs [destructuring](patterns.md) 
on its left-hand side.

[`defn`](../std/defn) is a similar shorthand for defining global functions:
	
	(defn brew (cauldron)
	  (push! cauldron 'eye-of-newt))

	; ...is equivalent to...

	(bind-global! 'brew (fn &name brew (cauldron)
	  (push! cauldron 'eye-of-newt)))

And we have [`defmacro`](../std/defmacro) for global macros:

	(defmacro with-pointy-hat (..body)
	  `(do
	    (put-on-hat)
	    ~..body
	    (remove-hat)))

	; ...is equivalent to...

	(bind-macro! 'with-pointy-hat (fn &name with-pointy-hat (..body)
	  `(do
	    (put-on-hat)
	    ~..body
	    (remove-hat))))

In both cases, a `&name` clause is used to assign a symbol as the function's name for debugging 
purposes. This means that `defn` and `defmacro` generally lead to a nicer debugging experience, 
compared to using `bind-global!` and `bind-macro!` directly.


## Recursive Local Functions

Defining a local function which calls itself recursively can be awkward:
	
	(let recurse (fn (n)
	  (cond 
	    ((> n 5)
	      (recurse (- n 1)))
	    (else
	      (prn n)))))

	(recurse 10.5) ; error: unbound symbol 'recurse'

The outer call to `recurse` works as expected, but the `let` binding is not in scope for the
recursive call, so it attempts to access a global variable named `recurse` instead.

The [`let-fn` macro](../std/let-fn) solves this problem:
	
	(let-fn recurse (n)
	  (cond 
	    ((> n 5)
	      (recurse (- n 1)))
	    (else
	      (prn n))))

	(recurse 10.5) ; prints 4.5

This works by initializing the local variable to `#n` first, and then immediately assigning the
function to it, so that the local binding is in scope throughout the function's body. It's 
equivalent to:
	
	(let recurse)
	(= recurse (fn (n) ...))


## Assignment

In GameLisp, all variables and collections are mutable by default.

The [`=` macro](../std/set) can be used to assign a new value to a **place**. A place might be a 
global or local variable, an array element, a table value, or any number of other things.

Simple use of `=` looks like this:
	
	(let a 10, b 20, c 30)

	(= a 40)
	(= b 50, c 60)

	(prn (+ a b c)) ; prints 150

When `=`'s first argument is a function call, it will inspect the name of the function, then 
replace the entire `=` form with a call to the corresponding "setter" function.
	
	(= (macro 'brew) (fn () #n))
	(= (global 'cursed?) #t)

	; ...is equivalent to...

	(macro= 'brew (fn () #n))
	(global= 'cursed? #t)

### Naming Conventions

Functions which assign a value to some memory location are suffixed with `=`, while functions
which perform other kinds of mutation are suffixed with `!`.
	
	; reads as "set the global cursed? to #t"
	(global= 'cursed? #t)

	; reads as "push the new-cat to the arr-of-cats"
	(push! arr-of-cats new-cat)

	; reads as "increment the cauldron-weight"
	(inc! cauldron-weight)

Where possible, it's good style to use the `=` macro, rather than calling a setter function 
directly. The `=` macro is easier to spot when visually scanning through the source code.

### In-Place Mutation

It's a very common operation to read the value currently stored in a place, pass it to a function 
(perhaps with some other arguments), and then assign the function's return value to that same 
place. We provide a number of macros to make this easier.
	
	(let n 0)
	(inc! n 5) ; add 5 to n
	(mul! n 2 2 2) ; multiply n by 2, three times
	(prn n) ; prints 40

[`inc!`](../std/inc-mut), [`dec!`](../std/dec-mut), [`mul!`](../std/mul-mut), 
[`div!`](../std/div-mut), [`div-euclid!`](../std/div-euclid-mut), [`rem!`](../std/rem-mut), 
[`rem-euclid!`](../std/rem-euclid-mut), [`abs!`](../std/abs-mut) and [`clamp!`](../std/clamp-mut) 
can be used to perform basic arithmetic in-place.

The [`(swap! a b)` macro](../std/swap-mut) swaps the values stored in any two places.
	
	; the two places can be different from one another
	(swap! my-local-var (global 'my-global-var))
	(swap! [my-arr 0] [my-arr -1])


## Arrows

The [`->`](../std/arrow-first) and [`->>`](../std/arrow-last) macros perform a simple syntax 
transformation which can make deeply-nested function calls easier to read.

The `->` macro accepts a form followed by any number of arrays or symbols. The form is evaluated 
as normal, and it's then passed through a chain of function calls, where the return value of
each call acts as the first argument to the next call.

	(fourth (third (second (first a) b) c))

	; ...could be refactored into...

	(-> (first a) (second b) (third c) fourth)

	; ...which is equivalent to...

	(do
	  (let tmp-1 (first a))
	  (let tmp-2 (second tmp-1 b))
	  (let tmp-3 (third tmp-2 c))
	  (fourth tmp-3))

The `->>` macro is similar, except that it passes in each form's result as the *last* argument
to the following call, rather than its first argument.
	
	(fourth (third c (second b (first a))))

	; ...could be refactored into...

	(->> (first a) (second b) (third c) fourth)

	; ...which is equivalent to...

	(do
	  (let tmp-1 (first a))
	  (let tmp-2 (second b tmp-1))
	  (let tmp-3 (third c tmp-2))
	  (fourth tmp-3))

When you have a several function calls deeply nested inside one other, it's usually possible to 
refactor them with either `->` or `->>`. Having data flow from start to finish can be much 
easier to follow, compared to data which travels up and down a call-tree.
	
	(+ ..(map (fn (n) (* n 2)) (filter even? (arr 1 2 3 4))))

	; ...could be refactored into...

	(->> 
	  (arr 1 2 3 4)
	  (filter even?)
	  (map (fn (n) (* n 2)))
	  ..+)

It's a little like a chain of method calls. In Rust, the above would be written as:

```rust	
[1, 2, 3, 4]
	.filter(is_even)
	.map(|n| n * 2)
	.sum()
```
