#![cfg_attr(not(feature = "unsafe-internals"), forbid(unsafe_code))]
#![allow(clippy::comparison_chain)]
#![allow(clippy::float_cmp)]
#![allow(clippy::useless_format)]
#![feature(min_specialization)]
#![feature(rustc_attrs)]
#![feature(unboxed_closures)]
#![doc(html_root_url = "https://docs.rs/glsp/0.2")]

#[macro_use]
mod error;

#[macro_use]
mod val;

#[macro_use]
mod wrap;

#[macro_use]
mod collections;

#[macro_use]
mod engine;

mod ast;
mod class;
mod code;
mod compile;
mod encoder;
mod eval;
mod gc;
mod iter;
mod lex;
mod parse;
mod print;
mod serde;
mod transform;
mod vm;

pub use self::{
    class::{Class, Obj},
    code::{Coro, CoroState, GFn},
    collections::{
        Arr, Deque, DequeAccess, DequeAccessRange, DequeIndex, DequeOps, DequeRange, FromElement,
        IntoElement, IterDeque, IterDequeTo, IterTab, IterTabKeys, IterTabKeysTo, IterTabTo,
        IterTabValues, IterTabValuesTo, Splay, Str, Tab, TabEntries,
    },
    engine::{
        with_lazy_val, EprWriter, PrWriter, RClass, RClassBuilder, RData, RFn, RGc, RGlobal,
        RGlobalRef, RGlobalRefMut, RRef, RRefMut, RRoot, Sym, ToSym,
    },
    error::{GError, GResult},
    eval::{EnvMode, Expander, Expansion},
    gc::{Allocate, Gc, GcVal, GcVisitor, Root, GC_DEFAULT_RATIO, GC_MIN_RATIO},
    iter::{GIter, GIterLen, Iterable, IterableOps},
    val::{Hashable, Num, Val},
    wrap::{Callable, CallableOps, FromVal, IntoCallArgs, IntoVal, Rest, WrappedCall, Wrapper},
};

pub use self::engine::glsp::*;

//undocumented apis required by the glsp-stdlib crate or by macros
#[doc(hidden)]
pub use self::{
    engine::{stock_syms, Engine, EngineBuilder, Span, SymKind},
    gc::Slot,
    parse::Parser,
    print::{dump_fn, dump_form, dump_macro},
};
