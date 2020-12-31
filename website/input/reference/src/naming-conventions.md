# Naming Conventions

This appendix describes some of the naming conventions I've developed while working on
The Castle on Fire. Improvisation and experimentation are encouraged, but you might find
these guidelines to be a useful starting point.


## Guidelines

Prefer brevity.

The `=` suffix is for functions/methods which perform an assignment, the `!` suffix is for 
functions/methods which perform any other kind of destructive mutation, and the `?` suffix is for 
boolean variables and for functions/methods which return booleans. For conversions, consider
using `src->dst` for a function or `->dst` for a method.

The `+` suffix should be used for functions/methods which yield. In the unlikely event that
a yielding method performs an assignment or a destructive mutation, the `=` or `!` suffix
should be omitted.

`lower-kebab-case` is used for local variables, `let-fn` functions, `let-macro` macros, built-in 
functions, built-in macros, class fields, class clauses, and keyword symbols like `'ok`. If you're 
defining a function or macro which is intended to be very global, such as a new control-flow macro 
or a new numeric function, you should similarly use unprefixed `lower-kebab-case`.

`UpperCamelCase` is used for classes, states and `RData` types. This includes mixins, structs,  
local classes defined with `let-class`, and variables which store classes. Class names should
avoid prefixes where possible: `Sprite` rather than `GfxSprite` or `gfx:Sprite`. Capitalization 
follows the same rules as for a Rust struct, e.g. `HudPopup` rather than `HUDPopup`. 

An `RData`'s constructor function should be a global variable, named to resemble a class constructor: 
`(Sprite rgb w h)`, `(PhysicsRect coords)`. If an `RData` or a class has multiple possible 
constructors, globally bind a function to the type name, plus a suffix: `(Sprite:load path)`, 
`(TileLayer:from-arr tiles)`. In general, when manipulating `RData` and objects, prefer methods over 
free functions.

Global functions and global variables are categorized into de-facto modules with very short 
names, like `img`, `phys` or `res`. Global names are prefixed with their "module": 
`img:draw`, `res:load-resources`. Toplevel `let` variables and `let-fn` functions are similarly 
prefixed, mostly to differentiate them from local variables.
	
	(defn phys:step ()
	  ...)

	(let-fn phys:box-coords (box)
	  ...)

A small number of ubiquitous global variables are prefixed with `:`, purely to make them more 
brief. For example, `:clock` for the current game-time, `:dt` for the "delta time" since the last 
tick, and `:screen-w` for the pixel width of the back buffer.
