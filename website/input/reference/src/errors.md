# Errors

GameLisp's error-handling story is quite minimalist. Errors silently bubble up through the 
call-stack; they can be caught by the Rust API or caught within GameLisp code; and uncaught errors
print a stack trace and an error message. Internally, they're implemented using [`Result`].

[`Result`]: https://doc.rust-lang.org/std/result/

	(defmacro add-5 (form)
	  `(+ 5 ~form))

	(defn recursive (n)
	  (cond
	    ((> n 0)
	      (recursive (- n 1)))
	    (else
	      (add-5 'symbol))))

	(recursive 2)

	#|
	    stack trace:
	        glsp::load("example.glsp")
	        (recursive) at example.glsp:11
	        (recursive) at example.glsp:7
	        (recursive) at example.glsp:7
	        (add-5) at example.glsp:9
	            expanded to (+) at example.glsp:2
  
	    error: non-number passed to a numeric op
	|#

You can manually trigger an error by calling [`bail`](../std/bail) or [`ensure`](../std/ensure), 
which resemble Rust's `panic!()` and `assert!()` respectively. They accept any number of arguments 
to describe their error message. When two or more error-message arguments are present, those 
arguments are converted to a string, as though they had been passed to the [`str`
function](../std/str). 
	
	(bail "expected {expected-type} but received {(type-of arg)}")

	(ensure (>= [stats 'level] 10) "level is too low. stats: " (pretty-stats))

`ensure` doesn't actually evaluate its error-message arguments unless an error occurs, so it's 
safe to use expensive function calls when describing the error.


## Error Recovery

In game development, closing down the release-build executable when an error occurs should be the
last resort - something only done for unrecoverable errors. For some errors, it's safe to simply
log the fact that an error occurred, and then continue executing.

Consider code which loops through each of your game's entities and calls a function to render
them to the screen. If you catch any error produced by an entity's rendering function, and then
move on to the next entity as normal, it could potentially change a catastrophic bug (the
game crashing) into a minor one ("due to a calculation error, the fire elemental's particle effect
sometimes disappears").

You can catch errors within GameLisp code using the macros [`try`](../std/try) and 
[`try-verbose`](../std/try-verbose):
	
	(for entity in draw-list
	  (match (try (draw-entity entity))
	    (('ok result)
	      #n)
	    (('err payload)
	      (log-error entity payload))))

`try` evaluates its child forms within an implicit `do` block. When no error occurs, it returns
the two-element array `(ok result)`, where the first element is the symbol `ok`, and the second 
element is the result of evaluating its last child form.

If an error occurs within the `try` form's dynamic scope - including errors generated internally
by GameLisp, a `Result::Err` being returned by a Rust function, or even a `panic!()` within a
Rust function - the `try` macro will catch the error and return `(err payload)`, where the first 
element is the symbol `err` and the second element is a value which represents the error.

When `bail` or `ensure` are called with a single argument, that argument will not be converted
into a string. This means that `payload` can have any type. You could potentially use it to
set up a more structured and formal error-handling scheme.
	
	(let ('err payload) (try (bail 100)))
	(prn (int? payload)) ; prints #t

	(bail (tab
	  ('error-kind 'missing-resource-error)
	  ('filename "space-station.lvl")
	  ('resource-name 'laser-rifle)
	  ('recoverable #t)))

`try-verbose` is identical to `try`, but when an error occurs it returns
`(err payload stack-trace)`, where `stack-trace` is a string describing the call-stack when the
error was generated. Stack-trace generation can cost a lot of time, sometimes in excess of one 
millisecond, so `try-verbose` should be used sparingly.

### Exception Safety

There is a downside to capturing errors. If a function makes a series of changes which are
globally visible (like mutating an object field, mutating a global variable, or enabling or
disabling a state), an error could cause the function to suddenly stop executing, even if those 
changes are only partially complete. Capturing that error would leave your game in a buggy, 
incoherent state.

It's technically possible to guarantee coherency by using language features like 
[`defer`](#cleanup), but maintaining [exception safety] across your whole codebase would be a 
huge engineering challenge. For the average game codebase, it's certainly not worth the effort.

[exception safety]: https://en.wikipedia.org/wiki/Exception_safety

Instead, it's usually best to only capture errors when they originate from "read-only" code 
which doesn't mutate any global data. This might include your rendering code, or the function 
which deserializes a level file, or the function which writes a saved game to the file system. 
When you do capture an error, you should capture it as far down the call stack as possible, to 
limit the number of function calls which it might disrupt.


## Debugging

GameLisp's debugging facilities are not yet very mature.

The [`dbg` macro](../std/dbg) works like Rust's `dbg!()`: each argument's line number, form and 
return value are printed to the standard error stream.
	
	(let variable 10)
	(dbg (+ 2 3) variable)

	#|
	    prints:

	    [example.glsp:2] (+ 2 3) = 5
	    [example.glsp:2] variable = 10
	|#

Likewise, the [`todo` macro](../std/todo) is designed to resemble Rust's `todo!()`. It calls 
`(bail)` with an error message along the lines of `"not yet implemented"`.

The [`file-location` function](../std/file-location) returns a brief filename and line number 
as a string, like `"scripts/somewhere.glsp:42"`. 

The [`stack-trace` function](../std/stack-trace) returns a full stack-trace string, as described 
above. 


## Cleanup

You'll be familiar with the use of [`Drop`] in Rust to clean up resources like file handles
and mutex locks.

[`Drop`]: https://doc.rust-lang.org/std/ops/trait.Drop.html

This type of scoped resource-handling is less common in GameLisp, but you might still need
to execute cleanup code from time to time. This can be achieved using the [`defer` special
form](../std/defer). `defer` executes a number of forms when control exits from its enclosing 
lexical scope, whether that's because of normal execution, `return`, `continue`, `break`, 
`restart-block`, `finish-block`, or an uncaught error.
	
	; prints: first second third fourth
	(defer (prn "fourth"))
	(do
	  (defer (pr "third "))
	  (do
	    (defer (pr "second "))
	    (pr "first "))
	  (bail)
	  (prn "this line is unreachable"))

[`yield`](../std/yield) is more complicated. With `yield`, it's possible to leave a lexical scope, 
and then return to it later on using `coro-run`. Many other languages simply don't perform cleanup 
when yielding out of a coroutine, which can lead to unexpected resource leaks. To avoid this,
we provide the [`defer-yield` special form](../std/defer-yield), which executes one form every 
time a `yield` exits its lexical scope, and another form every time `coro-run` resumes into its 
lexical scope.

The combination of `defer` and `defer-yield` is powerful. They allow you to guarantee that a
particular condition holds for the entire duration of a dynamic scope (that is, the forms which
fall within a particular lexical scope, and any functions which those forms call, and any 
functions called by those functions...). We use this to provide the [`with-global` 
macro](../std/with-global), which overrides the value of a global variable for the duration of a
dynamic scope:
	
	(defmacro with-global (name value-form)
	  `(splice
	    (let old-value# (global '~name))
	    (let new-value# ~value-form)
	    (= (global '~name) new-value#)
	    (defer
	      (= (global '~name) old-value#))
	    (defer-yield
	      (do
	        (= new-value# (global '~name))
	        (= (global '~name) old-value#))
	      (do
	        (= old-value# (global '~name))
	        (= (global '~name) new-value#)))))

	(defn hello-world ()
	  (prn "hello, world"))

	; make all prn calls much more exciting within a dynamic scope
	(let boring-prn prn)
	(with-global prn (fn (..args) (boring-prn ..args "!!!")))
	
	(hello-world) ; prints hello, world!!!

`with-global` is as powerful as [Racket's parameters] and [Common Lisp's dynamic variables], but we 
were able to implement it as a simple macro, without any special support from the language.

[Racket's parameters]: https://docs.racket-lang.org/guide/parameterize.html
[Common Lisp's dynamic variables]: http://www.gigamonkeys.com/book/variables.html#dynamic-aka-special-variables


## Syntax Information

When an array is parsed from a file, it's tagged with a small amount of hidden syntax information 
which indicates the file and line number from which it was parsed. This syntax information is 
essential for reporting a meaningful backtrace when an error occurs.

You normally won't need to think about this. Macros, `expand` and `backquote` automatically
generate appropriate syntax information for you.

The exception is when you manually transform one array into another within a macro - for example, 
by calling `(arr ..(rev source-arr))`. The `arr` function has no way of knowing that its result 
should be syntactically similar to `source-arr`, so syntax information is not preserved.

One solution is to `clone` the source array, and then mutate the resulting array in-place,
perhaps by `clear!`ing it and then rebuilding it from scratch. `clone` preserves syntax 
information, so the new array will have the same syntax information as the source array.

Alternatively, the [`map-syntax` function](../std/map-syntax) takes an array and a mapping 
function, and returns a new array with the same syntax information, where each element was 
created by calling `(mapper source-element)`.
