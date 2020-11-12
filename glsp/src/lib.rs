/*!
The [GameLisp](https://gamelisp.rs) scripting language.

For a guided tour of the Rust API, see Section 2 of the 
[Reference Manual](https://gamelisp.rs/reference/overview.html).
*/

pub use glsp_engine::*;

pub use glsp_proc_macros::*;

pub use glsp_stdlib::*;

#[cfg(feature = "compiler")]
pub use glsp_proc_macros2::*;

pub mod prelude {
	/*!
	The prelude.
	
	Due to a limitation in Rust's name lookup, the prelude currently imports any functions which
	share their name with a macro. For example, it imports both the macro [`arr!`](macro.arr.html)
	and the function [`glsp::arr`](fn.arr.html).
	
	This is unintentional, and it may change in the future. When using the prelude, macros can 
	be invoked without their prefix, but functions should still be invoked with their
	`glsp::` prefix.
	*/

	#[doc(no_inline)]
	pub use crate::{
		arr, backquote, bail, bail_at, ensure, ensure_at, 
		epr, eprn, error, lib, macro_no_op, pr, prn, quote,
		rdata, rdata_impls, rfn, str, sym, syms, tab, try_arr, 
		try_backquote, try_tab,

		Arr, 
		Callable, CallableOps, Class, Coro, CoroState, 
		Deque, DequeAccess, DequeAccessRange, DequeOps, 
		EnvMode, Expander, Expansion,
		FromVal, 
		GError, GFn, GIter, GIterLen, GResult,
		Hashable, 
		Iterable, IterableOps,
		Lib, LibRef, LibRefMut, 
		Num, 
		Obj, OrNil,
		RData, RFn, Root, RRoot, RRef, RRefMut, Runtime, RuntimeBuilder,
		Splay, Str, Sym,
		Tab, ToSym, ToVal, 
		Val,
	};

	#[cfg(feature = "compiler")]
	#[doc(no_inline)]
	pub use crate::{compile, eval};
}
