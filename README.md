# GameLisp

**GameLisp** is a scripting language for [Rust](https://www.rust-lang.org) game development.

To get started, take a look at the [homepage](https://gamelisp.rs). Please note that GameLisp
currently requires the latest version of **nightly** Rust.

_**Update, March 2022**: My game project is on hold, meaning that GameLisp is not currently under 
active maintenance. Due to [issue #36](https://github.com/fleabitdev/glsp/issues/36), the `glsp`
crate will fail to compile when using nightly Rust toolchains newer than 30th September 2021._

## Contributing

Bug reports and pull requests are welcome. All contributions submitted for inclusion in the work 
will be dual-licensed as described below, with no additional terms or conditions.

## Roadmap

Short-term goals for the language, in descending order of priority:

- Fix [issue #36](https://github.com/fleabitdev/glsp/issues/36)
- Add test suite
- Add syntax-highlighting for more text editors
- Report the location of mismatched parentheses when parsing
- Work off some technical debt
    - Make gc timing more consistent when `"unsafe-internals"` is disabled
    - Optimize `forn`, collections, objects, and the write-barrier
    - Forbid names like `iter` from being bound as local variables
- Add built-in functions for debugging
    - Convenient functions for running a REPL in-game
    - Some level of support for hotloading
- Audit the crate's integer conversions and overflow (e.g. `usize as u32` casts)
- Add multiple-value operations for the `DequeOps` trait, like `set_slice` and `starts_with`
- Add bindings to the `regex` and `chrono` crates, behind feature flags
- Experiment with reflection, raw classes or metaclasses, as potential replacements for mixins
  and classmacros

## License

This project is licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
  [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](LICENSE-MIT) or
  [http://opensource.org/licenses/MIT](http://opensource.org/licenses/MIT))

at your option.
