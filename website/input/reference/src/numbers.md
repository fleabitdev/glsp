# Numbers

Compared to other Lisps, GameLisp's numbers are relatively simple. A number may only be an `int`
(a 32-bit signed integer) or a `flo` (a 32-bit IEEE float). Rational fractions, arbitrary-precision 
integers and complex numbers are not supported.

The API for manipulating numbers is mostly unsurprising - the full list of functions is available
[here](../std/numbers). Some minor points:

- Bit manipulation is supported for integers.

- We provide a simple random number generator.

- There is a distinction between `sign` (which returns `0`, `1` or `-1` for any number) and
  `flo‑sign` (which returns the sign of a float as `1.0`, `-1.0` or `nan.0`).

<span></span>

	(prn (sign -0.0)) ; prints 0
	(prn (flo-sign -0.0)) ; prints -1.0


## Promotion to Float

Floats are "contagious". When a function such as [`+`](../std/add) receives both an integer and a 
float among its arguments, the integer is promoted to a float before performing the operation, so 
the return value is always a float.
	
	(prn (+ 1 2 3 4)) ; prints 10
	(prn (+ 1 2 3.0 4)) ; prints 10.0

	(prn (/ 7 2)) ; prints 3
	(prn (/ 7 2.0)) ; prints 3.5

	(prn (% 5 1.5)) ; prints 0.5

Operations like `min` and `clamp` are the exception to the rule, because they return one of
their arguments unchanged.

	(prn (max 1 2 3.0 4)) ; prints 4
	(prn (clamp 1.0 3 5.0)) ; prints 3


## NaN

According to the IEEE 754 specification, all comparisons involving NaN floats should return 
false. This means that NaN floats are unequal to themselves, and they're neither less than nor 
greater than any other number.

This rule is inconvenient when sorting numbers, and when working with hash tables, priority
queues, and sorted arrays. GameLisp therefore imposes a total ordering on floats: NaN floats 
compare greater than all other floats (including positive infinity), and all NaN floats compare 
equal to all other NaN floats.


## Wrapping Arithmetic

Integer arithmetic is always unchecked (wrapping), even when your crate is compiled in debug
mode.
	
	(prn (+ 1 2147483647)) ; prints -2147483648

This helps to keep the language simple. If GameLisp were to take Rust's approach to integer 
overflow, it would need to provide distinct APIs for normal, wrapping and checked arithmetic.
In Rust, this adds a [large complexity burden][0] when working with integers.

[0]: https://doc.rust-lang.org/std/primitive.i32.html

GameLisp chose wrapping arithmetic over checked arithmetic in order to make fatal errors less 
likely. In game development, overflowing an entity's coordinates and getting a nonsensical value 
will usually be a minor bug, but overflowing an entity's coordinates and crashing the game's 
executable would be catastrophic.
