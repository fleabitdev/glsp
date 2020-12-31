# Object-Oriented Programming

Game code is naturally object-oriented. A typical game codebase might contain:
	
	- A central "engine", mostly implemented in a native language, to handle anything highly 
	  performance-sensitive (such as physics and rendering) and anything which is inherently 
	  global (such as key bindings and saved games).
	
	- A very large number of unique "entity" types, mostly implemented in a scripting language,
	  each representing a different kind of in-game object.
		- The engine behaves as a "server" and the entities behave as its "clients": registering 
		  themselves to participate in the physics system, emitting drawing commands, 
		  serializing themselves when the game is saved, and so on.

		- Entities are sometimes data-driven (for example, a monster in an action-adventure game
		  might be partially defined by its total health and a list of elemental weaknesses), but 
		  defining a new entity usually involves at least some scripting (for example, each monster 
		  would have an AI script to control its behaviour in combat).

Entity definitions usually make up the lion's share of any game's codebase, so providing a great
environment for defining entities was my number one priority while designing GameLisp. After 
experimenting with a few [alternatives](#aside-why-not-prototypes), I eventually settled on a 
traditional, class‑based object‑oriented programming (OOP) system.

This may surprise some readers - after all, OOP has been steadily going out of fashion for the
last decade or two, particularly in game development. This is because OOP has a few well-known
flaws which can make it architecturally awkward. GameLisp has a unique take on OOP which
tries to minimize those downsides, while still taking advantage of its convenience and 
intuitiveness. It's quite different from OOP as seen in languages like C++, Java and Python.


## Fundamentals

GameLisp's [primitive types](syntax-and-types.md#type-summary) include objects and classes.
Each class describes a type of object, and each object is an individual instance of a class.

You can define a new class using the macros [`defclass`](../std/defclass), 
[`let-class`](../std/let-class) and [`class`](../std/class). They work in the same way as 
`defn`, `let-fn` and `fn`, respectively.

	(defclass Bomb
	  (const initial-time 30.0)
	  (field timer @initial-time)

	  (met tick (delta-time)
	    (dec! @timer delta-time)
	    (when (<= @timer 0.0)
	      (prn "BOOM!"))))

Classes are full-fledged GameLisp values. You can store them in an array, bind them to a local
variable, pass them as a function argument, and so on. `defclass` just evaluates a `(class ...)` 
form and binds it to a global variable.

	(let Renamed Bomb)
	(ensure (class? Bomb))
	(ensure (same? Bomb Renamed))

	(prn Bomb) ; prints #<class:Bomb>
	(del-global! 'Bomb)
	(prn Bomb) ; an error

Classes are always immutable. There's no way to modify a class after you've defined it.

As demonstrated above, classes are described using a sequence of "clause" forms. The most 
important clauses are [`(field ...)`](../std/field-clause), 
[`(const ...)`](../std/const-clause) and [`(met ...)`](../std/met-clause).

### Fields and Constants

A [`(field name)` clause](../std/field-clause) defines an object field. Each field reserves enough 
space inside the object to store a single GameLisp value, of any type. You can optionally provide 
an initializer form, which will be evaluated during [object initialization](#initialization). All 
fields are mutable.

A [`(const name init)` clause](../std/const-clause) defines an associated constant. Constants take 
up no storage in the object. Their `init` form is evaluated when the class is defined, and it can 
refer to previous constants in the same class.
	
	(const width 40)
	(const height 60)
	(const area (* @width @height))

Fields and constants share the same namespace, and they're public by default. You can access a 
field or constant on a particular object using the [`[]` syntax](../std/access), just as you might 
access a table field.
	
	(defn describe-bomb (bomb)
	  (prn "initial time: " [bomb 'initial-time])
	  (prn "current time: " [bomb 'timer]))

	(defn reset-bomb (bomb)
	  (= [bomb 'timer] [bomb 'initial-time]))

You can also use `[]` to look up the value of a constant in a class.
	
	(prn "initial time for all bombs: " [Bomb 'initial-time])

### Methods

A [`(met name (args...) body...)` clause](../std/met-clause) defines a method: a function which 
"acts on" a particular object. Unlike Rust, the `self` argument is defined implicitly rather than 
explicitly, so you would write `(met a (b) ...)` rather than `(met a (self b) ...)`.

Methods share the same namespace as fields and constants, and like fields and constants, they're 
public by default.

To invoke a method on an object, use the `(.name obj args...)` syntax.
	
	(defn tick-all-bombs (bombs delta-time)
	  (for bomb in bombs
	    (.tick bomb delta-time)))

That same syntax can be used to invoke a function which is stored in a field or constant. In that 
case, the function isn't considered to be a method, so it won't receive a `self` parameter and it
won't be able to access any object fields.
	
	(defclass Static
	  (const print-description (fn () 
	    (prn "your hair is standing on end!"))))

	(.print-description Static)


## `@` Forms

`@name` is an [abbreviation](syntax-and-types.md#abbreviations) for 
[`(atsign name)`](../std/atsign).

`@` is only meaningful within the body of a method, or within the initializer form of a field or 
constant. Broadly speaking, it's used to access some property of the current object, similar to 
`self` in Rust. 

`@width` means "access the field or constant named `width` on the current object".

`(@run a b)` means "invoke the method `run` on the current object, with arguments `a` and `b`".
	
	(defclass Paintbrush
	  (field red)
	  (field green)
	  (field blue)

	  (met rgb ()
	    (arr @red @green @blue))

	  (met print-color ()
	    (prn (@rgb)))

	  (met make-grayscale! ()
	    (let avg (/ (+ @red @green @blue) 3))
	    (= @red avg, @green avg, @blue avg)))

There are a [few special cases](../std/objects-and-classes#abbreviations). `@self` returns a 
reference to the current object, `@class` is the current object's class, and `@class-name` is 
the name of the current object's class as a symbol.

### First-Class Functions

Methods can't be passed around as first-class values. If you try to access a method as you would
access a field or constant, it's an error.

	(defclass WidgetValidator
	  (const threshold 10.0)

	  (met widget-valid? (widget)
	    (>= [widget 'validity] @threshold))

	  (met validate (widgets)
	    (all? @widget-valid? widgets))) ; an error

However, forms like `@self` and `@width` work by referring to a hidden local variable, which is
eligible to be captured using `fn`. Therefore, creating a first-class function which delegates to 
a method is straightforward.
	
	(met validate (widgets)
	  ; invoke the widget-valid? method on @self, passing in each widget
	  (all? (fn1 (@widget-valid? _)) widgets))


## Initialization

To instantiate a new object, simply call a class, in the same way that you would call a function.
	
	(let bomb (Bomb))

	(prn bomb) ; prints #<obj:Bomb>
	(ensure (obj? bomb))
	(ensure (same? (class-of bomb) Bomb))

You can pass in parameters to the function call. Those parameters will be passed to a special
initializer method, which can be defined using an [`(init ...)` clause](../std/init-clause).
	
	(defclass Refinery
	  (field fuel-limit)
	  (field fuel)

	  (init (@fuel-limit)
	    (= @fuel (/ @fuel-limit 5))))

	(let refinery (Refinery 1000))
	(prn [refinery 'fuel]) ; prints 200

There will often be a one-to-one relationship between your class's fields and the parameters to
its initializer method. To save you from typing out the same name several times, we provide
special field-initialization syntax:
	
	(defclass AppleTree
	  (field fruit-count)
	  (field health (* @fruit-count 20))
	  (field planted-on-date)

	  (init (@fruit-count @planted-on-date))
	    (prn "created an apple tree with {@fruit-count} fruit"))

	(let tree (AppleTree 6 (in-game-date)))

The initialization method may prefix any of its parameter names with `@`, in which case the class 
must have a field which shares the same name. At the start of the initialization method, each 
field is initialized in the order that they were defined, emitting an `(= @name name)` form for 
any parameters prefixed with `@`.

In other words, the above `AppleTree` class definition is equivalent to:
	
	(defclass AppleTree
	  (field fruit-count)
	  (field health)
	  (field planted-on-date)

	  (init (fruit-count planted-on-date)
	    (= @fruit-count fruit-count)
	    (= @health (* @fruit-count 20))
	    (= @planted-on-date planted-on-date)
	    (prn "created an apple tree with {@fruit-count} fruit")))

Normal `met` clauses may also have `@`-parameters. In that case, they just emit a `(= @name name)`
form at the start of their body, in no particular order.
	
	(met on-health-change (@health)
	  (when (< @health 0)
	    (prn "the tree withers away!")))

	; ...is equivalent to...

	(met on-health-change (health)
	  (= @health health)
	  (when (< @health 0)
	    (prn "the tree withers away!")))


## Finalization

The [`obj-kill!` function](../std/obj-kill-mut) will execute an object's finalizer method 
(defined using a [`(fini ...)` clause](../std/fini-clause)), and then permanently delete 
its storage. Trying to access a field or call a method on a killed object is an error.
	
	(defclass Mandrake
	  (field coords)

	  ; ...

	  (fini
	    (for entity in (query-entities 'within-distance 50 @self)
	      (.hit entity 150 'necrotic))))

	(let mandrake (Mandrake spawn-coords))
	(obj-kill! mandrake)
	(prn [mandrake 'coords]) ; an error

Note that the `fini` method will not be called if an object is simply garbage-collected,
so it can't be used for RAII-style resource cleanup. We'll discuss some alternatives to RAII
in the [Errors](errors.md) chapter.


## Privacy

If you read the textbooks, they will tell you that information hiding is one of the pillars
of object-oriented programming. Languages like C# tend to make all class members private
by default - not only is it impossible to access them from a different project, but they can't
even be accessed by a different class within the same source file!

In practice, for day-to-day code, it's easy to waste too much effort carefully hiding fields 
and methods from yourself and your colleagues. I find that a game codebase can tolerate a lot of 
"unnecessarily public" fields and methods, as long as you choose sensible names for those few 
bindings which are intended to be accessed from outside the class. This is why fields, constants 
and methods are public by default in GameLisp.

That being said, privacy is occasionally useful. If you're writing a [mixin](code-reuse.md#mixins) 
or [classmacro](#classmacros) which will be integrated into many different classes, or if you're
writing a class for a library which is intended to be used by strangers, then putting lots of 
names in your object's public namespace becomes significantly more risky.

In the macros chapter, we discussed a technique for avoiding name collisions in macros:
[auto‑gensym](macros.md#hygiene). Classes have access to the same technique. When a symbol
anywhere in a class is suffixed with `#`, each occurrence of that symbol will be replaced with 
the same [`gensym`](../std/gensym). This makes it impossible to refer to that name from outside 
the class, but it can still be accessed by other objects of the same class.
	
	(defclass SecretivePoint
	  (field x#)
	  (field y#)

	  (init (@x# @y#))

	  (met op-eq? (other)
	    (and (== @x# [other 'x#]) (== @y# [other 'y#]))))

	(let point (SecretivePoint 20 20))
	(prn [point 'x#]) ; an error


## Properties

If you have one entity which advertises its coordinates as a field `[et 'coords]`, and a more 
complex entity which provides the same information as a method `(.coords et)`, writing client code 
to deal with both possibilities can be irritating.

On the other hand, when defining a class, you don't want to waste a lot of effort writing useless
methods which only return a field's value.
	
	(defclass GameLispOrJava?
	  (field width# 5)
	  (field height# 5)

	  (met width ()
	    @width#)

	  (met height ()
	    @height#))

GameLisp borrows a leaf from C#'s book by allowing you to define a pair of methods which behave 
like a field. The "getter" method is called when the field is accessed, and the "setter" method is
called when a new value is assigned to the field.
	
	(defclass GameLispOrCSharp?
	  (prop width 5 (get))
	  (prop height 5 (get)))

Each property has a "backing field" which stores a single value. The class above would define
one field named `width:field` and another named `height:field`. The empty `(get)` forms simply
return the backing field's current value. You could also define an empty `(set)` form, which just 
assigns its argument to the backing field.

Of course, `(get ...)` and `(set ...)` can also be defined with a method body. Within those 
methods, it's possible to refer to the backing field as [`@field`](../std/atsign-field):
	
	; a creature whose apparent position differs from its true position
	(defclass DisplacerBeast
	  (prop coords
	    (get
	      (let (x y) @field)
	      (arr (- x 10) (- y 10)))
	    (set (arg)
	      (let (x y) arg)
	      (= @field (arr (+ x 10) (+ y 10)))))

	  (met print-description ()
	    (prn "my coords are " @coords))

	  (met print-secret-description ()
	    (prn "my actual coords are " @coords:field)))

Just like fields and constants, properties can be initialized automatically, using the syntax
`(prop name initializer-form ...)`. The initial value is assigned to the backing field directly; 
the setter is not invoked.

Properties are more complicated than fields, so you should avoid using them when they're not
necessary. Unless you really need to prevent assignment for some reason, the classes above 
should simply be written as:
	
	(defclass GameLisp
	  (field width 5)
	  (field height 5))


## Arrows

As usual, the [arrow macros](built-in-macros.md#arrows) can be used to flatten out deeply-nested
function calls and field accesses. The arrow macros include special handling for `@name` and 
`.name` forms.
	
	(-> rect .coords (.offset-by 10 10) @to-local ['x])

	; ...is equivalent to...

	[(@to-local (.offset-by (.coords rect) 10 10)) 'x]

Notice the resemblance to Rust's method invocation syntax:
	
```rust
rect.coords.offset_by(10, 10)
```


## Classmacros

Class definitions can become repetitive. For example, every entity in your game might have
an `on-step` method with the same set of parameters:
	
	(defclass MetalWall
	  (met on-step (controller delta-time)
	    ...))

	(defclass LaserSword
	  (met on-step (controller delta-time)
	    ...))

	; i've only typed that boilerplate twice and i'm already tired of it

You can use `defclassmacro` to define a macro which will be invoked in place of a class clause.
For example, in the above case, we might define a `step` classmacro:
	
	(defclassmacro step (..body)
	  `(met on-step (controller delta-time)
	    ..body))

	(defclass MetalWall
	  (step
	    ...))

	(defclass LaserSword
	  (step
	    ...))

Classmacros can use [`splice`](macros.md#the-expansion-algorithm) to emit multiple clauses from a 
single macro invocation. On the other hand, if a classmacro decides that it doesn't want to emit 
any clauses, it can return `#n`. Nil clauses are silently ignored.

Classmacros are powerful. For example, you could define a small `coro-step` classmacro which 
resumes a coroutine every step.
	
	(defclassmacro coro-step (..body)
	  `(splice
	    (field coro-name#)
	    (field setter-name#)

	    (met on-step (controller cur-delta-time#)
	      (when (or (nil? coro-name#) (not (eq? (coro-state coro-name#) 'paused))) 
	        (let delta-time cur-delta-time#)
	        (= coro-name# ((fn () ..body)))
	        (ensure (coro? coro-name#))
	        (= setter-name# (fn (dt) (= delta-time dt))))
  
  	      (setter-name# cur-delta-time#)
  	      (coro-run coro-name#))))

	(defclass LaserSword
	  (coro-step
	    (prn "vrumm")
	    (yield)
	    (prn "VWOM")
	    (yield)
	    (prn "ñommmm")))


## Aside: Why Not Prototypes?

Several popular scripting languages (namely Lua, JavaScript, and their derivatives) have an object
system very different from GameLisp's. In those languages, there is no distinction between an 
object and a table, or between a method and a table field. Tables may delegate field accesses to 
another table, which establishes a *de facto* single inheritance hierarchy.

Prototype-based object systems have the advantage of being extremely simple and easy to learn.
Unfortunately, they're also quite limited. Some pre-release versions of GameLisp experimented 
with a prototype-based object system, and encountered these problems:

- Prototype chains tend to have poor performance, and the nature of the object model makes
  optimization difficult.
- The hybrid object/tables tend to be weakly-typed and excessively dynamic. There's often no way
  to disable unhelpful operations like adding arbitrary fields to any object, or deleting any 
  field, or iterating over every field. Abstractions end up feeling leaky and insecure.
- The lack of dedicated syntax for things like methods and fields causes many quality-of-life
  papercuts. Methods being called as non-method functions accidentally; silent name collisions;
  no easy way to differentiate a general-purpose hash table from an object; no good way to make
  an individual table field immutable; etc.
- Prototypes are a natural fit for single inheritance, but a poor fit for more complicated kinds 
  of class relationship, like those which we'll explore in the next two chapters.
