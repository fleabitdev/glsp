# Feature Flags

By default, the `glsp` crate's only transitive dependencies are [`smallvec`], [`owning_ref`], 
[`stable_deref_trait`], [`fnv`], and the Rust standard library.

	$ cargo tree
	glsp v0.1.0
	+-- glsp-engine v0.1.0
	|   +-- fnv v1.0.7
	|   +-- owning_ref v0.4.1
	|   |   +-- stable_deref_trait v1.1.1
	|   +-- smallvec v1.4.0
	+-- glsp-proc-macros v0.1.0
	|   +-- glsp-engine v0.1.0 (*)
	+-- glsp-stdlib v0.1.0
		+-- glsp-engine v0.1.0 (*)
		+-- glsp-proc-macros v0.1.0 (*)
		+-- smallvec v1.4.0 (*)

All large or non-essential dependencies are feature-gated, and all features are disabled by
default.

[`smallvec`]: https://docs.rs/smallvec
[`owning_ref`]: https://docs.rs/owning_ref
[`stable_deref_trait`]: https://docs.rs/stable_deref_trait
[`fnv`]: https://docs.rs/fnv


## "unsafe-internals"

By default, `glsp`'s implementation doesn't use any `unsafe` code at all. This is guaranteed 
using `#![forbid(unsafe_code)]`.

With the `"unsafe-internals"` feature enabled, a small amount of unsafe code is switched on in
the `glsp-engine` crate. This makes the interpreter run roughly [twice as 
fast](performance-figures.md).

Note that `glsp`'s public API is always intended to be safe, even when the `"unsafe-internals"`
feature is enabled. The purpose of this feature flag is to mitigate the safety impact of any 
undetected bugs which are internal to the `glsp` crate.

Even with `"unsafe-internals"` disabled, `glsp` may depend on crates which themselves use `unsafe` 
internally - currently [`smallvec`], [`owning_ref`] and optionally [`bincode`]. As usual, 
you shouldn't trust this crate's safety unless you also trust its dependencies.

[`bincode`]: https://docs.rs/bincode


## "serde"

Introduces a dependency on the [`serde`] crate, but not [`serde_derive`].

Implements [`Serialize`] and [`Deserialize`] for [`Val`], [`Root`], [`Arr`], [`Tab`], [`Str`] and 
[`Sym`]. Note that the serializer will gracefully fail if it encounters a [non-representable] 
type, or a type which contains reference cycles. [Gensyms](../std/gensym) and textually-ambiguous 
symbols can be serialized and deserialized, even though they're not representable.

[`serde`]: https://docs.rs/serde
[`serde_derive`]: https://docs.rs/serde_derive
[`Serialize`]: https://docs.serde.rs/serde/ser/trait.Serialize.html
[`Deserialize`]: https://docs.serde.rs/serde/de/trait.Deserialize.html
[`Val`]: https://docs.rs/glsp/*/glsp/enum.Val.html
[`Root`]: https://docs.rs/glsp/*/glsp/struct.Root.html
[`Arr`]: https://docs.rs/glsp/*/glsp/struct.Arr.html
[`Tab`]: https://docs.rs/glsp/*/glsp/struct.Tab.html
[`Str`]: https://docs.rs/glsp/*/glsp/struct.Str.html
[`Sym`]: https://docs.rs/glsp/*/glsp/struct.Sym.html
[non-representable]: strings-and-text.html#parsing-and-unparsing


## "compiler"

Introduces a dependency on the `"serde"` feature, as well as the crates [`bincode`], [`flate2`], 
[`syn`], [`quote`], [`proc_macro2`] and [`serde_derive`].

This feature flag enables GameLisp source code to be pre-compiled into an efficient binary 
format. Provides the [`compile!`] and [`eval!`] macros, and the [`glsp::load_and_compile`] 
and [`glsp::load_compiled`] functions. See the [Compilation](compilation.md) chapter for
more information.

[`flate2`]: https://docs.rs/flate2
[`syn`]: https://docs.rs/syn
[`quote`]: https://docs.rs/quote
[`proc_macro2`]: https://docs.rs/proc_macro2
[`compile!`]: https://docs.rs/glsp/*/glsp/macro.compile.html
[`eval!`]: https://docs.rs/glsp/*/glsp/macro.eval.html
[`glsp::load_and_compile`]: https://docs.rs/glsp/*/glsp/fn.load_and_compile.html
[`glsp::load_compiled`]: https://docs.rs/glsp/*/glsp/fn.load_compiled.html
