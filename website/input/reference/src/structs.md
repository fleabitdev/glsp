# Structs

Objects are fast and space-efficient, especially when they don't contain any `state` forms. If 
you have a small compound data structure (say, a geometric primitive, or the return value from a 
function), it's usually best to implement it as a class, rather than a table or an array.

	(defclass Rect
	  (field x)
	  (field y)
	  (field w)
	  (field h)

	  (met init (@x @y @w @h))

	  (met area ()
	    (* @w @h))

	  (met op-clone ()
	    (Rect @x @y @w @h))

	  (met op-eq? (other)
	    (let [x y w h] other)
	    (and (== @x x) (== @y y) (== @w w) (== @h h))))

That's a lot of code for something so simple! We provide the `defstruct` macro to get rid of
the boilerplate:
	
	(defstruct Rect 
	  x y w h

	  (met area ()
	    (* @w @h)))

	(prn Rect) ; prints #<class:Rect>
	(let rect (Rect:new 10 10 20 20))
	(prn rect) ; prints #<obj:Rect>
	(prn [rect 'x]) ; prints 10
	(prn (.area rect)) ; prints 400

As demonstrated above, the `defstruct` macro defines a class with the given named fields. After 
the list of field names, `defstruct` also accepts zero or more `met`, `prop` and `const` 
clauses. Other class clauses, like `init`, `state`, `fsm`, `wrap` and `mixin`, are forbidden.
Classmacros are also forbidden.


## Struct Initialization

When programming in Rust, you will have encountered tuple structs: structs which identify their
fields by position, rather than by name.

```rust
struct Raster {
	pixels: Vec<u32>,
	width: u32,
	height: u32,
	format: ImageFormat
}

// vs.

struct Raster(Vec<u32>, u32, u32, ImageFormat);
```

Tuple structs with more than one field are generally discouraged. They're hard to understand, 
hard to refactor, and they require more documentation. This is doubly true in a dynamically-typed 
language. For example, if you wanted to change the order of a tuple's fields in a Python
codebase, the language would give you no help at all; it would be a completely manual task.

To encourage the use of named (rather than positional) struct fields, `defstruct` registers a 
global initialization macro which shares the struct's name. This macro behaves like a Rust struct 
initializer, or like the `tab` macro:

	(defstruct Hit
	  hitbox
	  strength
	  element)

	(let hitbox (Rect @x @y w h))

	(let fire-punch (Hit
	  hitbox
	  (strength 35)
	  (element 'fire)))

	(let ice-punch (Hit
	  (element 'ice)
	  ..fire-punch))

	; the Hit macro resembles a table constructor
	(let fire-punch-table (tab
	  ('hitbox hitbox)
	  ('strength 35)
	  ('element 'fire)))

The original class is bound to the global `Name:new`. This enables you to easily opt in to
using positional arguments, when you think they would be the better choice.
	
	(defstruct Rgb r g b)

	(let named (Rgb (r 32) (g 139) (b 32)))
	(let positional (Rgb:new 32 139 32))

	(prn (eq? named positional)) ; prints #t

Finally, the global `Name?` is bound to a function which tests whether or not a value is
a `Name` struct. This is more convenient and intuitive than writing `(is? val Name)` every
time.
	
	(let chartreuse (Rgb 0x7f 0xff 0x00))
	(prn (Rgb? chartreuse)) ; prints #t


## Operator Overloading

The default behaviour of the `eq?` function, when comparing two objects, is to test them for
[identity](miscellaneous.md#equality) using the `same?` function. This means that two objects
can belong to the same type, and store the same values, but still compare unequal to one
another.

You can override this default behaviour by defining a method named `op-eq?`.

As noted above, `op-eq?` is automatically implemented by the `defstruct` macro. It will compare 
each struct field in turn using `eq?`.
	
	(defclass Spawner
	  (field level) ; a large, immutable table
	  (field to-spawn) ; a class
	  (field remaining) ; an integer counter

	  (met op-eq? (other)
	    (and
	      (same? @level [other 'level])
	      (same? @to-spawn [other 'to-spawn])
	      (== @remaining [other 'remaining]))))

By default, the `clone` and `deep-clone` functions only duplicate a reference to an object;
they don't copy the object's storage. You can provide `op-clone` and `op-deep-clone` methods to
override this behaviour.
