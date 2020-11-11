# Changelog

## Unreleased

### Changed

- Improved error message when glsp functions are called with no active `Runtime`

### Fixed

- `(int)` and `(flo)` would not accept characters when called as an operator
- `RData` destructors triggered panics every time they interacted with the runtime
- The `syms!` macro required some names to be in scope, and emitted an incorrect struct name

## Version 0.1 

Initial release.
