# Strings and Text

In GameLisp, a string is an array which can only store characters.

In fact, strings support the full array API described in the [previous chapter](arrays.md):
`len`, `push-start!`, `remove!`, indexing with integers, slicing, and so on. The only difference
is that assigning a non-character value to a string is an error.

This enables you to write code which is generic over both strings and arrays. For example,
a function which reverses a string or array in-place:

    (defn rev! (deq)
      (ensure (deque? deq))

      (forn (i (/ (len deq) 2))
        (swap! [deq i] [deq (- i)]))

      deq)

Notice that we test for the type `deque`, which is the [abstract type](syntax-and-types.md#type-summary) 
implemented by anything which supports an array-like interface. For a string, 
[`arr?`](../std/arr-p) would return `#f`, but [`deque?`](../std/deque-p) returns `#t`.


## String Storage

Because our double-ended queue API requires constant-time random access, we can't encode 
strings using UTF-8: locating the nth character in a UTF-8 string is an `O(n)` operation.

Instead, we take a leaf out of [Python's](https://www.python.org/dev/peps/pep-0393/) book.
By default, a string will use a [`VecDeque<u8>`] for its backing storage. The first time a 
character with a scalar value above `255` is assigned to the string, it switches to a 
`VecDeque<u16>`. Similarly, any scalar value above `65535` will cause the storage to be 
converted to a `VecDeque<char>`.

This scheme has good performance characteristics. When compared to UTF-8, it typically uses 
equal or less storage, except when a string contains text from multiple scripts. Each string will 
change its character width at most two times; strings which only contain ASCII and the 
[Latin-1 Supplement] will never change their character width; and most non-Latin strings will 
typically change their character width zero or one times, rather than two.

[`VecDeque<u8>`]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html
[Latin-1 Supplement]: https://en.wikipedia.org/wiki/List_of_Unicode_characters#Latin-1_Supplement


## Converting Values to Strings

Any value can be converted to text using the [`str` function](../std/str). It accepts zero or more 
arguments; inserts spaces between adjacent non-character, non-string arguments; converts each 
of those arguments to text; and returns the concatenation of all of the arguments' text as a 
newly-allocated, mutable string.

    (str)                      ; ""
    (str \a \b \c)             ; "abc"
    (str 1 2 3)                ; "1 2 3": note the spaces
    (str 1 " " 2 " " 3)        ; "1 2 3": equivalent to the previous call
    (str "hello" \w "or" "ld") ; "helloworld": no spaces added between strs/chars

This is also how [`pr`](../std/pr) and [`prn`](../std/prn-fn) process their arguments. `prn` 
appends a UNIX-style line ending, `"\n"`, to its output.

The [`sym`](../std/sym) function is similar to `str`, but it doesn't insert spaces between any of 
its arguments, and it converts the result into a symbol. It's an error if the string is empty, or 
if it contains anything other than the [valid symbol characters](syntax-and-types.md#sym). You can
test this using the functions [`valid-sym-char?`](../std/valid-sym-char-p) and
[`valid-sym-str?`](../std/valid-sym-str-p).
    
    (valid-sym-str? "") ; #f
    (valid-sym-str? "hello-world") ; #t
    (valid-sym-str? "hello world") ; #f
    (valid-sym-str? "42.42") ; #t

    (prn (sym "suffixed-" 100)) ; prints suffixed-100
    (prn (sym "*invalid()\ncharacters[]")) ; an error

We also support [template strings](syntax-and-types.md#abbreviations). A template string evaluates 
to a newly-allocated, mutable string with values printed into it. It's like the `format!()`
macro in Rust, but more convenient.

    (let arg 2)
    (prn "1 + {arg} = {(+ 1 arg)}") ; prints 1 + 2 = 3

    ; within curly braces, adjacent values are separated by spaces
    (prn "{(+ 1 1) (+ 1 2)} 4 {(+ 1 4)}") ; prints 2 3 4 5

Finally, you'll sometimes want to customize how numbers are printed. 
[`(int->str i radix)`](../std/int-to-str) will convert an integer to a string with the given 
radix, and [`(flo->str f places)`](../std/flo-to-str) will convert a floating-point number to a 
string with the given number of digits after the decimal point.


## Non-Representable Values

In the first chapter of this section, we mentioned [representable values]. A representable value
is one which can be converted to a string, and then parsed back from that string, with no loss of
information.

[representable values]: syntax-and-types.md#representable-types

It's still possible to print non-representable values, or convert them to a string. The printer
will usually prefix them with `#<` and suffix them with `>`, to make it obvious that they can't be
parsed.

    (prn (gensym)) ; prints #<gs:0>
    (prn (arr type-of +)) ; prints (#<rfn:type-of> #<rfn:+>)
    (prn (fn () #n)) ; prints #<fn>


## Parsing and Unparsing

The parser can be invoked manually using the [`parse-all`](../std/parse-all) function, which 
receives a string as its argument, and returns an array of all of the values parsed from that 
string. It's an error if the string contains invalid syntax.

When you know that the input contains exactly one form, [`parse-1`](../std/parse-1) will parse
and return that form.
    
    (parse-all "1 (a b)") ; returns the array (1 (a b))
    (parse-all "hello") ; returns the array (hello)
    (parse-1 "hello") ; returns the symbol hello

You'll sometimes have data which you want to store as text and then read back in later - for
example, in a savegame or a configuration file. Under those circumstances, it's important that
the data is representable. You'll need to avoid the following:
    
- Values which belong to a non-representable type, such as functions or iterators
- A reference cycle, which would cause the printer to get stuck in an endless loop
- Symbols like `-10` or `..name`, which will be read back in as numbers or abbreviations
- Symbols generated using [`gensym`](../std/gensym), including [`backquote`](../std/backquote)'s 
  `auto-gensym#` feature

You'll also need to double-quote and escape strings, and convert characters to their literal 
representation, i.e. printing the string `"\a"` rather than the character `\a`.

Checking all of these conditions every time would be tedious, so we provide a function
[`unparse`](../std/unparse) which does the work for you. It's similar to `str`, but it guarantees 
that if the resulting string is passed to `parse-all`, the parsed values will be equal
to `unparse`'s arguments.

    (prn (unparse "w" \x (arr 'y 'z))) ; prints "w" \x (y z)
    (prn (str "w" \x (arr 'y 'z))) ; prints wx(y z)

    (let non-repr-sym (sym "42"))
    (prn (str non-repr-sym)) ; prints 42
    (prn (unparse non-repr-sym)) ; an error


## Output Streams

By default, `pr` and `prn` send their output to the standard output stream.

We also provide [`epr`](../std/epr) and [`eprn`](../std/eprn), which are identical except that 
their output goes to the standard error stream.

It's possible for the host Rust program to customize these functions so that they send their
output somewhere else - we'll discuss that in [Section 2](the-glsp-crate.md#output-streams).


## Pretty-Printing

Although GameLisp's syntax is easy enough to write, the raw data is not very pleasant to read 
when printed:

    (prn '(cond
      ((>= (len ar) 2)
        (run [ar 0] [ar 1]))
      (else
        (run [ar 0]))))

    ; prints (cond ((>= (len ar) 2) (run [ar 0] [ar 1])) (else (run [ar 0])))

GameLisp comes with a simple pretty-printer which attempts to format data/code with reasonable
whitespace. It's not gold-standard, but it's usually good enough for debugging.

    (pretty-prn '(cond
      ((>= (len ar) 2)
        (run [ar 0] [ar 1]))
      (else
        (run [ar 0]))))

    #|
      prints:

      (cond
        ((>= (len ar) 2) (run [ar 0] [ar 1]))
        (else (run [ar 0])))
    |#

All of the pretty-printing functions only accept a single value, which they convert to a pretty
string with no leading or trailing whitespace. Those functions are 
[`pretty‑str`](../std/pretty-str), [`pretty‑unparse`](../std/pretty-unparse), 
[`pretty‑prn`](../std/pretty-prn) and [`pretty‑eprn`](../std/pretty-eprn).
