# Changelog

## Unreleased

### Added

### Changed

### Fixed

- `cargo clippy` triggered many warnings

## Version 0.2 (2nd January 2021)

### Added

- Added the `sym!` macro, as a convenient alternative to `glsp::sym(x).unwrap()`
- Added `glsp::load_str` and `(load-str)`, as a convenient alternative to `parse-all`
  followed by `eval-multi`
- Defined a total ordering for floats: NaNs now compare equal to other NaNs, and compare greater 
  than all non-NaN numbers
- Added `sort` and `sort_by` methods to the `DequeOps` trait
- Added `is_representable` and `is_serializable` methods to `Arr`, `Tab`, `Sym` and `Val`
- Added the `glsp::is_representable_sym` function
- The `backquote!` macro can now unquote local variables by reference, `~&var_name`
- Added `Gc` weak pointers (along with `GcVal`, `RGc`, `RClassBuilder::trace` and 
  `glsp::write_barrier`) to permit `RData` to store pointers to other heap-allocated objects
- The reference manual now suggests a `+` suffix for functions which `yield`
- Added a syntax-highlighting package for Visual Studio Code

### Changed

- The `GSend` and `GStore` auto traits have been removed
    - The `optin_builtin_traits` and `negative_impls` nightly features are no longer required
    - There is no longer any restriction on variables captured or returned by `Runtime::run`
    - `Root`, `RGlobalRef`, `RGlobalRefMut`, `RRef` and `RRefMut` can now be stored in an `RData`
- `RFn`, `RData`, Rust globals, and the function-wrapping code have been overhauled
    - The `min_specialization`, `rustc_attrs` and `unboxed_closures` nightly features are 
      now required
    - The `RStore` trait and `rdata!` macro have been removed. `RData` may now store any
      `'static` Rust type
    - Associating an `RClass` with a Rust type is now a dynamic operation, using `RClassBuilder`
    - The `Lib` trait has been renamed to `RGlobal`, and the `lib!` macro has been removed
    - `RFn`s are now stored on the garbage-collected heap, as `Root<RFn>`
    - The `rfn!` macro has been removed. Function pointers and closures can now be passed directly
      to `glsp::rfn` and similar functions
    - Capturing closures can now be passed to `glsp::rfn`, as long as they're `'static`
    - Rest parameters are now captured using a wrapper type `Rest<T>`, rather than a slice `&[T]`
    - Optional parameters will now be set to `None` when their argument is `#n`
- `meth`, `has-meth?`, `meth-name` and `call-meth` have been renamed to `met`, `has-met?`,
  `met-name` and `call-met` respectively
- Improved error message when glsp functions are called with no active `Runtime`
- The `ord` function now accepts strings, symbols and arrays
- `sort`'s comparison function now defaults to `ord`
- `PartialOrd`, `Ord` and `Eq` are now implemented, where appropriate, for `Val`, `Num`, `Root`,
  `Arr`, `Str`, `Tab` and `Sym`
- Removed the `free!` function. `RData::free` can still be called from Rust code
- The `rand-select` and `chance` functions have been renamed to `rand-pick` and `chance?`
- The `coin-flip` function has been removed
- `glsp::is_valid_sym_str` has been renamed to `glsp::is_valid_sym`
- `IntoVal`, `FromVal` and `IntoCallArgs` are now implemented for arrays of any length
- The `syms!` macro now defines a `new()` method which returns `Self`, rather than `GResult<Self>`

### Fixed

- `(int)` and `(flo)` would not accept characters when called as an operator
- `RData` destructors triggered panics every time they interacted with the runtime
- The `syms!` macro required some names to be in scope, and emitted an incorrect struct name
- `RGlobal::borrow` and `RGlobal::borrow_mut` had incorrect error messages under some 
  circumstances
- Calling `glsp::take_rglobal` would cause a panic in `Heap`'s destructor
- The parser would panic when it encountered a string which contained a multi-byte character
- `(defer)` and `(defer-yield)` would overwrite some local variables and scratch registers

## Version 0.1 (11th June 2020)

Initial release.
