# Changelog

## Unreleased

### Added

- The `rdata!` and `lib!` macros now support `enum` declarations as well as `struct` declarations
- Added the `sym!` macro, as a convenient alternative to `glsp::sym(x).unwrap()`
- Defined a total ordering for floats: NaNs now compare equal to other NaNs, and compare greater 
  than all non-NaN numbers
- Added `sort` and `sort_by` methods to the `DequeOps` trait

### Changed

- `meth`, `has-meth?`, `meth-name` and `call-meth` have been renamed to `met`, `has-met?`,
  `met-name` and `call-met` respectively
- Improved error message when glsp functions are called with no active `Runtime`
- The `ord` function now accepts strings, symbols and arrays
- `sort`'s comparison function now defaults to `ord`
- `PartialOrd`, `Ord` and `Eq` are now implemented, where appropriate, for `Val`, `Num`, `Root`,
  `Arr`, `Str`, `Tab` and `Sym`
- The `rand-select` and `chance` functions have been renamed to `rand-pick` and `chance?`
- The `coin-flip` function has been removed

### Fixed

- `(int)` and `(flo)` would not accept characters when called as an operator
- `RData` destructors triggered panics every time they interacted with the runtime
- The `syms!` macro required some names to be in scope, and emitted an incorrect struct name
- `Lib::borrow` and `Lib::borrow_mut` had incorrect error messages under some circumstances
- Calling `glsp::take_lib` would cause a panic in `Heap`'s destructor

## Version 0.1 

Initial release.
