# GameLisp

**GameLisp** is a scripting language for [Rust](https://www.rust-lang.org) game development.

To get started, take a look at the [homepage](https://gamelisp.rs). Please note that GameLisp
currently requires the latest version of **nightly** Rust.

## Contributing

The most direct way to support the project's continued development is via 
[Patreon](https://www.patreon.com/fleabitdev). If you've found GameLisp to be useful or 
interesting, please consider contributing!

Bug reports and pull requests are welcome. All contributions submitted for inclusion in the work 
will be dual-licensed as described below, with no additional terms or conditions.

## Roadmap

Short-term goals for the language, in descending order of priority:

- A test suite
- Making the crate Clippy-friendly
- Syntax-highlighting for more text editors
- Reporting the location of mismatched parentheses when parsing
- Working off some recent technical debt
    - Make gc timing more consistent when `"unsafe-internals"` is disabled
    - Optimize `forn`, collections, objects, and the write-barrier
    - Forbid names like `iter` from being bound as local variables
- Built-in functions for debugging
    - Convenient functions for running a REPL in-game
    - Some level of support for hotloading
- Auditing the crate's integer conversions and overflow (e.g. `usize as u32` casts)
- Bindings to the `regex` and `chrono` crates, behind feature flags
- Generalizing `let-macro` - this will enable things like classmacros, patterns and struct
  definitions to be lexically scoped
- User-defined patterns - this will enable structs to participate in pattern-matching
- Multiple-value operations for the `DequeOps` trait, like `set_slice` and `starts_with`
- Adding an additional, larger demo to the playground
- Experimenting with reflection, raw classes or metaclasses, as potential replacements for mixins
  and classmacros

## License

This project is licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
  [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](LICENSE-MIT) or
  [http://opensource.org/licenses/MIT](http://opensource.org/licenses/MIT))

at your option.
