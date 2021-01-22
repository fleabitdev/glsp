# Evaluation

In the [previous chapter](syntax-and-types.md), we introduced the fact that GameLisp code is
made out of a tree of simple values. Those values are sometimes parsed directly from a text file, 
but they can also be generated dynamically.

Unlike Rust, GameLisp code does not need to be compiled into a binary before it can be run.
You just pass your code to GameLisp, and it's executed straight away.

This chapter will describe precisely what happens when GameLisp is asked to run a particular 
value as a piece of code.


## Basic Evaluation

"Evaluating" a value means executing it as a piece of code. When we're talking about evaluation,
we tend to say "form" rather than "value", but they're just two different names for the same 
thing. When a form is evaluated, it either returns another value or triggers an error.

Values of type `nil`, `bool`, `int`, `flo`, `char`, `str` or `tab` are self-evaluating. If you 
evaluate a form of one of these types, no code is executed; the "evaluation" simply returns the 
form itself.

Symbols evaluate to the current value of the variable which they name. Local variable bindings 
take precedence over global bindings. If there are no local or global bindings for the symbol
which is being evaluated, it's an error.

Arrays are a little more complicated:

- Empty arrays are self-evaluating.
    
- When the array's first element is a symbol which names a [special form](#special-forms), such 
  as `quote`, `if` or `do`, then that form's special evaluation rules are applied.

- Otherwise, the array is evaluated as a function call. All of its elements are evaluated in 
  order, from left to right. The result of evaluating the leftmost form becomes the
  "callee", the function which is being called. The results of evaluating all of the other 
  forms (if any) are passed to the call as its arguments. The function call's return value
  becomes the result of the evaluation.

Attempting to evaluate a form of any 
[non-representable type](syntax-and-types.md#representable-types) is an error.

A callee is usually a [function](#functions). Functions can be defined either in GameLisp or 
in Rust. GameLisp comes with a [very large number of built-in functions](../std/) in its
standard library.

For now, the main built-in functions you need to know are:

- [`prn`](../std/prn-fn) takes any number of arguments and prints their text representation to 
  stdout, separated by spaces and followed by a newline. [`pr`](../std/pr) does the same thing 
  without the newline. Both functions return `#n`.

- [`int?`](../std/int-p), [`tab?`](../std/tab-p) and [similar functions](../std/types) accept a 
  single argument. They return `#t` if the argument belongs to the specified type, or `#f` 
  otherwise.

- [`<`](../std/lt), [`==`](../std/num-eq), [`>=`](../std/gte) and so on perform numeric 
  comparisons. [`+`](../std/add), [`-`](../std/sub), [`%`](../std/rem) and so on perform basic
  arithmetic on numbers. It's an error to pass a non-number argument to any of these functions.
    - Note that GameLisp's arithmetic and comparisons use prefix notation rather than the
      more familiar [infix notation]. Rather than writing `1 + 2 + 3`, write `(+ 1 2 3)`; and
      rather than writing `i < len`, write `(< i len)`.

- [`arr`](../std/arr) constructs a new array which contains all of its arguments. For example, 
  `(arr)` will return a new empty array, and `(arr 1 2 3)` will return a new array which contains
  those three numbers.
    - You can "splay" the contents of one array into another with the abbreviation `..`; for
      example, `(arr 10 ..src)` will create a new array which contains the integer `10`,
      followed by all of the elements from the array `src`.

A few examples of nested function calls:
    
    (prn (> (+ 1 1 1) 2)) ; prints #t
    (prn (pr "a ") (pr "b ")) ; prints a b #n #n
    (prn (nil? #n) (int? 10) (bool? (int? "hello"))) ; prints #t #t #t
    (prn (nil? 1 2)) ; error: too many arguments

[infix notation]: https://en.wikipedia.org/wiki/Infix_notation


## Special Forms

"Special forms" are those which don't follow the basic evaluation rules described above. Because 
GameLisp's macro facilities are so powerful, it has a relatively small number of special forms,
compared to languages like Rust.

[`do`](../std/do) has the same effect as a block `{ }` in Rust. `(do a b c)` evaluates `a`, then
evaluates `b`, then finally evaluates `c`, returning its value. `(do)`, with no arguments, 
evaluates to `#n`.

Many other forms establish an "implicit `do`". They contain zero or more child forms which are
evaluated one after the other, and they return the result of evaluating their last child form.

[`quote`](../std/quote) suppresses evaluation: `(quote a)` returns the value of `a` without 
evaluating it. Remember that `quote` is usually [abbreviated](syntax-and-types.md#abbreviations) 
as `'`, so we would write `(quote val)` as `'val`. Quote is most often used either to stop a 
symbol from being evaluated as a variable, or to stop an array from being evaluated as a function
call:

    ; without the quotes, this would be evaluated as printing the values of 
    ; variables named "hello" and "world". with the quotes, it prints two
    ; symbols instead.
    (prn 'hello 'world)

    ; without the quote, this would be evaluated as a call to the function 
    ; named "alice", passing in the values of the variables "betty" 
    ; and "carlo" as arguments, and then printing the call's return value. 
    ; with the quote, it prints a literal array instead.
    (prn '(alice betty carlo))

[`if`](../std/if) performs conditional evaluation. It must always receive three forms: a 
"condition" form, a "then" form, and an "else" form. First, the "condition" form is evaluated. 
If its result is [truthy](syntax-and-types.md#bool), the "then" form is evaluated and returned; 
otherwise, the "else" form is evaluated and returned. 
    
    ; prints 5, but doesn't print 4
    (prn (if (> 2 1) 
      5 
      (prn 4)))


## Local Variables

GameLisp's local variables are lexically scoped and always mutable. They can be declared either
at the toplevel of a source file, or within an [implicit `do`](#special-forms).

Just like Rust, you declare a new local variable by using the [`let` special form](../std/let).
Its first argument should be a symbol, and its optional second argument is a form which will be
evaluated to initialize the variable. When the second argument is absent, the variable's initial 
value will be `#n`.

    (let a 20)
    (let b (* 20 a))
    (prn b) ; prints 400

When `let` has three or more arguments, it's equivalent to multiple consecutive `let` forms
with one or two arguments each. We could rewrite the above as:
    
    (let a 20, b (* 20 a))
    (prn b) ; prints 400

`let`'s first argument can be an arbitrary pattern rather than a symbol, but we'll discuss that 
in a [later chapter](patterns.md).


## Global Variables

When evaluating a symbol which isn't bound to a local variable, it's assumed to refer to a
global variable instead. The symbol `a-name` will evaluate to the current value of the
global variable binding for the symbol `a-name`, or it will trigger an error if there's no 
such binding.

GameLisp's global variables use [late binding]. This means that every time a global variable
is accessed or mutated, that variable's binding is looked up at the last possible moment. If
you're trying to access a global variable which doesn't exist, you won't find out when the code 
is loaded - the error is delayed until you actually try to access it. On the other hand, 
late binding means that there's no need to declare your variables in advance, so you won't
have to struggle with order-of-declaration problems or circular dependencies.

Global bindings can be dynamically created, destroyed, accessed or mutated by calling the built-in 
functions [`bind-global!`](../std/bind-global-mut), [`del-global!`](../std/del-global-mut), 
[`global`](../std/global) and [`global=`](../std/set-global).

Built-in function calls like `(prn 'hello)` make use of global variables. When the GameLisp runtime
starts up, the symbol `prn` is bound to a global variable. The initial value of that global
variable is set to a Rust function which prints its arguments.

A local variable "shadows" a global variable which is bound to the same name:
    
    (prn 'hello) ; prints hello
    (let prn 10)
    (prn 'world) ; error: callee is an int

Late binding enables some useful tricks. For example, if you're creating a 2D game and you find it
inconvenient to look up your sprites in a hash table, you could arrange for them to be
automatically bound to global variables instead:
    
    (draw spr:zombie-head (- x 10) (- y 10))
    (draw spr:zombie-body (- x 10) y)

[late binding]: https://en.wikipedia.org/wiki/Late_binding#Late_binding_in_Lisp
[forward declaration]: https://en.wikipedia.org/wiki/Forward_declaration


## Functions

Functions in GameLisp are like closures in Rust, in that they can be stored as the value of a 
variable. Functions defined in GameLisp have the type `fn`, while functions defined in Rust
have the type `rfn`.

The `fn` special form defines a new function, which can then be called just like one of the
built-in functions:

    (let triple (fn (n)
      (let doubled (+ n n))
      (+ n doubled)))

    (prn (triple 70)) ; prints 210

The body of a function establishes an "implicit `do`", like the `do` special form. Each of
its body forms are evaluated one after the other, and the value of the final form is returned as 
the result of the function call. You can return early using the [`return` special 
form](../std/return).

By default, a function only accepts a fixed number of arguments:
    
    (prn (triple)) ; error: too few arguments
    (prn (triple 1 2)) ; error: too many arguments

However, a fn's parameter list is a [pattern](patterns.md#functions). Among other things, this
allows you to describe an optional parameter with the special syntax `(? name default-val)`, or
capture zero or more arguments with the special syntax `..name`.
    
    (let sum-and-triple (fn (..nums)
      (* 3 (+ ..nums))))

    (prn (sum-and-triple 10 20 30)) ; prints 180

    (let print-multi (fn (item (? times 1))
      (forn (_ times)
        (pr item " "))
      (prn)))

    (print-multi 'badgers) ; prints badgers 
    (print-multi 'badgers 3) ; prints badgers badgers badgers 
    
Functions capture the [lexical environment][0] at their definition site. This means that you don't
need to worry about local variables going out of scope:
    
    (let f (do
      (let captured 10)
      (fn ()
        captured)))

    (prn (f)) ; prints 10, even though `captured` is out-of-scope

[0]: https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scoping
