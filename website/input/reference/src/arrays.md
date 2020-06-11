# Arrays

GameLisp's general-purpose sequential data structure is called an "array", but it's actually
a [`VecDeque`].

A `VecDeque` is a growable [ring buffer]. It's very similar to a `Vec`, but with the added ability 
to push and pop items from the start of the sequence in constant time. (Note that a `VecDeque`
is very different from  a C++ [`std::deque`](https://en.cppreference.com/w/cpp/container/deque), 
which has an odd memory-allocation patterns - `VecDeque` allocates all of its elements
is a single buffer, just like a `Vec`.)

[`VecDeque`]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html
[ring buffer]: https://en.wikipedia.org/wiki/Circular_buffer


## Basic Functions

The [`arr`](../std/arr) function will return a new array which contains all of its arguments. 
When one of the arguments is an array, and that argument is prefixed with `..`, all of that 
array's elements are copied into the array which is being constructed. 

	(prn (arr)) ; prints ()
	(prn (arr 1 2 3 4)) ; prints (1 2 3 4)

	(let src '(x y z))
	(prn (arr 1 2 src 3 4)) ; prints (1 2 (x y z) 3 4)
	(prn (arr 1 2 ..src 3 4)) ; prints (1 2 x y z 3 4)
	(prn (arr ..src ..src)) ; prints (x y z x y z)

[`len`](../std/len) returns the array's length, and [`empty?`](../std/empty-p) tests whether it 
has a length of 0.
	
	(prn (len '())) ; prints 0
	(prn (len (arr 'a 'b))) ; prints 2

	(prn (empty? (arr))) ; prints #t
	(prn (empty? '(1))) ; prints #f

An array constructed using the `arr` function will be mutable. You can add any number of elements
to the start or end of an array using the functions [`push-start!`](../std/push-start-mut) and 
[`push!`](../std/push-mut), or you can remove one element at a time using 
[`pop-start!`](../std/pop-start-mut) and [`pop!`](../std/pop-mut).

	(let metals (arr 'pewter 'silver 'copper))

	(push! metals 'iron 'bronze)
	(prn metals) ; prints (pewter silver copper iron bronze)

	(prn (pop! metals)) ; prints bronze
	(prn (pop! metals)) ; prints iron
	(prn (pop-start! metals)) ; prints pewter
	(prn metals) ; prints (silver copper)

	(push-start! metals 'titanium 'electrum)
	(prn metals) ; prints (titanium electrum silver copper)

Many other useful functions are described in the [standard library 
documentation](../std/collections).


## Indexing

The macro for looking up an element in any collection is called [`access`](../std/access). 
To get the first element of an array, you might call `(access the-array 0)`.

Because `access` is such a fundamental operation, it can be
[abbreviated](syntax-and-types.md#abbreviations) using square brackets. `(access the-array 0)`
would normally be written as `[the-array 0]` instead. Notice that this resembles the equivalent
Rust syntax: `the_array[0]`.

Negative indexes count backwards from the end of the array. `[names -1]` returns the last element
in the `names` array, and `[names -2]` returns its second-to-last element.

Array elements are [places](built-in-macros.md#assignment), so they can be mutated using the
`=` macro.
	
	(let blues (arr 'azure 'sapphire 'navy))

	(prn [blues 2]) ; prints navy
	(prn [blues -3]) ; prints azure

	(= [blues 0] 'cerulean)
	(= [blues -2] 'cobalt)

	(prn blues) ; prints (cerulean cobalt navy)


## Slicing

The `access` macro, and therefore the `[]` abbreviation, both support special syntax for accessing
multiple consecutive elements in an array. They use the `:` symbol, similar to Python's slice 
syntax.
	
	(let alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

	; elements from n to m are sliced using `n : m`
	(prn [alphabet 3 : 8]) ; prints (d e f g h)
	(prn [alphabet -10 : 21]) ; prints (q r s t u)
	(prn [alphabet 5 : 5]) ; prints ()

	; elements from 0 to n are sliced using `: n`
	(prn [alphabet : 5]) ; prints (a b c d e)
	(prn [alphabet : -23]) ; prints (a b c)
	(prn [alphabet : 30]) ; an error

	; elements from n to the end are sliced using `n :`
	(prn [alphabet 23 :]) ; prints (x y z)
	(prn [alphabet -1 :]) ; prints (z)

	; the entire array can be sliced using `:`
	(prn (eq? [alphabet :] alphabet)) ; prints #t

	; the `:` symbol is whitespace-sensitive
	(prn [alphabet 2:5]) ; an error
	(prn [alphabet 3:]) ; an error

To keep things simple, all of these slicing operations will allocate, and return, a new array. 
Unlike Rust, there's no way to produce a reference which points into an array's interior.

The [`del!`](../std/del-mut) and [`remove!`](../std/remove-mut) macros also support the same 
slicing syntax.
	
	(del! names 3) ; delete element 3
	(del! names 2 : 5) ; delete elements 2, 3 and 4
	(del! names :) ; delete every element

A slice is a [place](built-in-macros.md#assignment). Assigning an array to a slice will overwrite
all of the elements stored there, changing the size of the array if necessary.
	
	(let numbers (arr 0 1 2 3 4 5 6 7 8 9))

	(= [numbers : 6] '())
	(prn numbers) ; prints (6 7 8 9)

	(= [numbers -2 :] (arr 42 42 42))
	(prn numbers) ; prints (6 7 42 42 42)

	(= [numbers :] '(5 5 5))
	(prn numbers) ; prints (5 5 5)

GameLisp doesn't include some of the traditional Lisp functions for processing sequences, like 
`rest`, `butlast`, `take` and `drop`. All of those functions can be expressed in a more versatile 
and general way using the slice syntax.


## Arrows

Deeply-nested use of the `[]` syntax can sometimes be visually confusing.

	[[[the-array index] 0] start-index :]

"Arrow macros" were discussed in the [previous chapter](built-in-macros.md#arrows). The `[]`
syntax works well when used with the `->` macro:
	
	(-> the-array [index] [0] [start-index :])

This is similar to the equivalent Rust syntax:

	the_array[index][0][start_index..]
