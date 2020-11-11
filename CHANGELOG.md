# Changelog

## Unreleased

### Added

- The `rdata!` and `lib!` macros now support `enum` declarations as well as `struct` declarations

### Changed

- Improved error message when glsp functions are called with no active `Runtime`

### Fixed

- `(int)` and `(flo)` would not accept characters when called as an operator
- `RData` destructors triggered panics every time they interacted with the runtime
- The `syms!` macro required some names to be in scope, and emitted an incorrect struct name
- `Lib::borrow` and `Lib::borrow_mut` had incorrect error messages under some circumstances
- Calling `glsp::take_lib` would cause a panic in `Heap`'s destructor

## Version 0.1 

Initial release.
