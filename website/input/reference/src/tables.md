# Tables

GameLisp's main associative data structure is the table. Tables are [`HashMaps`] which can use 
arbitrary GameLisp data for their keys.

[`HashMaps`]: https://doc.rust-lang.org/std/collections/struct.HashMap.html

The basic operations are similar to those for an [array](arrays.md). You can read or write table
entries using square brackets, `[tbl key]`. Assignment will create an entry if it doesn't already
exist. The [`has?`](../std/has-p) function will tell you whether a key is already present, and 
the [`del!`](../std/del-mut) and [`remove!`](../std/remove-mut) functions will delete an existing 
entry. [`len`](../std/len), [`empty?`](../std/empty-p) and [`clear!`](../std/clear-mut) all work 
as expected.
    
    (let strengths (tab))
    (= [strengths 'goblin] 3)
    (= [strengths 'dragon] 8)

    (prn (has? strengths 'goblin)) ; prints #t
    (prn (has? strengths 'kobold)) ; prints #f

    (prn [strengths 'goblin]) ; prints 3
    (prn [strengths 'manticore]) ; an error

    (prn (len strengths)) ; prints 2
    (clear! strengths)
    (prn (empty? strengths)) ; prints #t


## Table Construction

Recall that the syntax for table literals is `#((key0 value0) (key1 value1))`.

To construct a new table dynamically, you can use the `tab` macro. It receives a number of array 
forms of length two, and optionally a number of forms which evaluate to tables, each prefixed 
with `..`. Each array form, and each entry from each of the tables, is treated as a `(key value)` 
pair which is inserted into the table.
    
    (let basic (tab ('a 'b) ('c 'd)))
    (let more (tab ('e 'f) ..basic))

    (prn more) ; prints #((a b) (c d) (e f)), not necessarily in that order

The [`extend!`](../std/extend-mut) function receives a table as its first argument, followed by 
any number of `(key value)` two-element arrays. Those key-value pairs are each inserted into the 
table, overwriting elements which already exist. It's typically used to copy the full contents of 
one table into another, by treating the source table as an [iterator](iterators.md):
    
    (extend! dst-table ..src-table)


## Nonexistent Elements

GameLisp is normally very strict when it comes to whether or not an element of a collection
exists. If you attempt to access a nonexistent table entry (or a nonexistent global, array index, 
object field, class field, or function parameter), it's an error.

This is in contrast to some other scripting languages, which return `nil` or `undefined` for
nonexistent elements. I find that this is not a sensible default: it can cause errors to
silently propagate, making refactoring and debugging more difficult.

If you need to access an element which may or may not exist, various macros support the special
syntax `(? form)`. This syntax can be used in place of a key or an index. It will cause the 
operation to succeed and return `#n` when an element is missing, rather than triggering an error.

    (let heights (tab ('mira 165) ('paul 178)))
    
    (prn [heights 'sara]) ; an error
    (prn [heights (? 'sara)]) ; prints #n

    (let ar (arr 10 20 30 40 50))

    (= [ar -8] -20) ; an error
    (= [ar (? -8)] -20) ; a silent no-op
    
    (prn (remove! ar 2)) ; prints 30
    (prn (remove! ar 7)) ; an error
    (prn (remove! ar (? 7))) ; prints #n
    
    (prn (global (? 'possibility))) ; prints #n
    (bind-global! 'possibility 100)
    (prn (global (? 'possibility))) ; prints 100


## Key Equivalence

All hash tables need to enforce an [equivalence relation] on their keys. They use this equivalence
relation to establish whether, when key B is inserted into the table, it should overwrite the entry
previously created for key A.

[equivalence relation]: https://en.wikipedia.org/wiki/Equivalence_relation

Our hash tables can use any GameLisp data as a key, including `#n` and NaN floats. The equivalence 
relation is represented by the function [`keys-eqv?`](../std/keys-eqv-p). This function is very 
similar to [`eq?`](miscellaneous.md#equality), with a few small changes:

- Numbers and characters act as distinct keys, even if they're numerically equal.
  `65`, `65.0` and `\A` are all `==` to one another, but they're not key-equivalent.

- For performance reasons, tables have to be compared for key-equivalence using 
  [`same?`](../std/same-p) rather than [`eq?`](../std/eq-p). This means that two tables can have 
  identical contents, but still be considered distinct when used as table keys.

- Objects and Rust data can overload `eq?`, but there's no way to overload `keys-eqv?`.

Otherwise, table keys mostly work as you would expect. Arrays and strings are key-equivalent when
they have the same contents. Other reference types are key-equivalent when they refer to the
same object. Value types are equivalent when they have the same type and the same contents.
