use super::ast::Ast;
use super::class::{Class, Obj};
use super::code::{Bytecode, Coro, GFn, Instr, ParamMap};
use super::collections::{Arr, DequeAccess, DequeOps, Str, Tab};
use super::engine::{glsp, stock_syms::*, RData, RFn, Span, Sym};
use super::error::GResult;
use super::eval::Expander;
use super::gc::{Allocate, Raw, Root, Slot};
use super::iter::GIter;
use super::val::Val;
use super::wrap::{CallableOps, IntoVal};
use super::{encoder, transform};
use smallvec::SmallVec;
use std::cmp::max;
use std::collections::HashMap;
use std::convert::From;
use std::fmt::{self, Debug, Display, Formatter, Pointer};
use std::iter::repeat;
use std::{char, str};

/*

this module is in charge of all conversions from glsp data to text. the rust apis are:
    - Display/Debug implementations for all types which represent glsp data (Val, Slot, Arr, Tab,
      Sym, Root, RData, etc). the output is human-readable, and unrepresentable values are replaced
      with a description surrounded by #<angle-brackets>, like #<class:ClassName> or #<rdata:Spr>.
        - the only difference between Display and Debug is that the Display implementation for
          Strs, Val::Strs, Slot::Strs, Val::Chars and Slot::Chars streams the string or char out verbatim,
          while the Debug implementation escapes and double-quotes strings, and escapes chars.
          this is consistent with rust's String/str/char types.
        - those Display/Debug implementations pretty-print their output when the '#' option is
          passed to the format string, e.g. format!("{:#}", some_val). this is identical to the
          usual Display/Debug output, except for additional whitespace.
    - a method check_representability for various types, to assert that they can take a round-trip
      through the Debug printer and the parser without losing any information. we use this method
      to enforce representability for anything which is unparsed.
*/

//-------------------------------------------------------------------------------------------------
// check_representability()
//-------------------------------------------------------------------------------------------------

/*

non-representable values are:
 - any reference type other than an array, string or table
   - an array or table which transitively stores a non-representable value
 - a symbol which will be parsed as a number or abbreviation: 42.0, ..aaa
 - a gensymmed symbol
 - an array or table which contains a reference cycle

check_representability() is actually the public entrypoint for the internal repr_test() method,
which keeps track of a stack of memory addresses to detect reference cycles.

we don't need to perform a similar check when evaluating a form. there's no reason for the
evaluator to reject ambiguous symbols or gensymmed symbols. reference cycles would be expensive
to detect in a naive way, and a less-naive approach would require special support from the
macro-expansion algorithm, essentially checking each array encountered by the algorithm against
each of its parents (todo?). ast::val_to_node and ast::quote_to_node ensure that
non-representable types can't be evaluated or transformed into literals.

for the same reason, we can't use check_representability() to screen a type for reference cycles
when passing it to serde. a literal register can contain gensyms or ambiguous syms, and the
compiler needs to serialize literals. instead, we provide a similar method check_serializability().

*/

impl Val {
    ///Returns `true` if this value can be losslessly converted to text.
    pub fn is_representable(&self) -> bool {
        self.check_representability().is_ok()
    }

    ///Returns `Ok` if this value can be losslessly converted to text.
    pub fn check_representability(&self) -> Result<(), &'static str> {
        let mut stack = SmallVec::new();
        self.repr_test(&mut stack, false)
    }

    ///Returns `true` if this value can be serialized and deserialized using Serde.
    ///
    ///This method is only present when the `"serde"` feature is enabled.
    #[cfg(feature = "serde")]
    pub fn is_serializable(&self) -> bool {
        self.check_serializability().is_ok()
    }

    ///Returns `Ok` if this value can be serialized and deserialized using Serde.
    ///
    ///This method is only present when the `"serde"` feature is enabled.
    #[cfg(feature = "serde")]
    pub fn check_serializability(&self) -> Result<(), &'static str> {
        let mut stack = SmallVec::new();
        self.repr_test(&mut stack, true)
    }

    fn repr_test(
        &self,
        stack: &mut SmallVec<[usize; 32]>,
        lenient_syms: bool,
    ) -> Result<(), &'static str> {
        match self {
            Val::Arr(arr) => arr.repr_test(stack, lenient_syms),
            Val::Tab(tab) => tab.repr_test(stack, lenient_syms),
            Val::Sym(sym) => {
                if lenient_syms {
                    Ok(())
                } else {
                    sym.check_representability()
                }
            }
            Val::Nil | Val::Int(_) | Val::Flo(_) | Val::Char(_) | Val::Bool(_) | Val::Str(_) => {
                Ok(())
            }
            Val::GIter(_) => Err("iters are non-representable"),
            Val::Obj(_) => Err("objects are non-representable"),
            Val::Class(_) => Err("classes are non-representable"),
            Val::GFn(_) => Err("functions are non-representable"),
            Val::RFn(_) => Err("functions are non-representable"),
            Val::Coro(_) => Err("coroutines are non-representable"),
            Val::RData(_) => Err("rust data is non-representable"),
        }
    }
}

impl Slot {
    pub fn check_representability(&self) -> Result<(), &'static str> {
        self.root().check_representability()
    }

    #[cfg(feature = "serde")]
    pub fn check_serializability(&self) -> Result<(), &'static str> {
        self.root().check_serializability()
    }
}

impl Arr {
    ///Returns `true` if this array can be losslessly converted to text.
    pub fn is_representable(&self) -> bool {
        self.check_representability().is_ok()
    }

    ///Returns `Ok` if this array can be losslessly converted to text.
    pub fn check_representability(&self) -> Result<(), &'static str> {
        let mut stack = SmallVec::new();
        self.repr_test(&mut stack, false)
    }

    ///Returns `true` if this array can be serialized and deserialized using Serde.
    ///
    ///This method is only present when the `"serde"` feature is enabled.
    #[cfg(feature = "serde")]
    pub fn is_serializable(&self) -> bool {
        self.check_serializability().is_ok()
    }

    ///Returns `Ok` if this array can be serialized and deserialized using Serde.
    ///
    ///This method is only present when the `"serde"` feature is enabled.
    #[cfg(feature = "serde")]
    pub fn check_serializability(&self) -> Result<(), &'static str> {
        let mut stack = SmallVec::new();
        self.repr_test(&mut stack, true)
    }

    fn repr_test(
        &self,
        stack: &mut SmallVec<[usize; 32]>,
        lenient_syms: bool,
    ) -> Result<(), &'static str> {
        let address = self as *const Arr as usize;
        if stack.contains(&address) {
            return Err("reference cycles are non-representable");
        }

        stack.push(address);
        for val in self.iter() {
            val.repr_test(stack, lenient_syms)?;
        }
        stack.pop().unwrap();

        Ok(())
    }
}

impl Tab {
    ///Returns `true` if this table can be losslessly converted to text.
    pub fn is_representable(&self) -> bool {
        self.check_representability().is_ok()
    }

    ///Returns `Ok` if this table can be losslessly converted to text.
    pub fn check_representability(&self) -> Result<(), &'static str> {
        let mut stack = SmallVec::new();
        self.repr_test(&mut stack, true)
    }

    ///Returns `true` if this table can be serialized and deserialized using Serde.
    ///
    ///This method is only present when the `"serde"` feature is enabled.
    #[cfg(feature = "serde")]
    pub fn is_serializable(&self) -> bool {
        self.check_serializability().is_ok()
    }

    ///Returns `Ok` if this table can be serialized and deserialized using Serde.
    ///
    ///This method is only present when the `"serde"` feature is enabled.
    #[cfg(feature = "serde")]
    pub fn check_serializability(&self) -> Result<(), &'static str> {
        let mut stack = SmallVec::new();
        self.repr_test(&mut stack, false)
    }

    fn repr_test(
        &self,
        stack: &mut SmallVec<[usize; 32]>,
        lenient_syms: bool,
    ) -> Result<(), &'static str> {
        let address = self as *const Tab as usize;
        if stack.contains(&address) {
            return Err("reference cycles are non-representable");
        }

        stack.push(address);
        for (key, value) in self.entries().iter() {
            key.repr_test(stack, lenient_syms)?;
            value.repr_test(stack, lenient_syms)?;
        }
        stack.pop().unwrap();

        Ok(())
    }
}

impl Sym {
    ///Returns `true` if this symbol can be losslessly converted to text.
    pub fn is_representable(&self) -> bool {
        self.check_representability().is_ok()
    }

    ///Returns `Ok` if this symbol can be losslessly converted to text.
    pub fn check_representability(&self) -> Result<(), &'static str> {
        if self.is_gensym() {
            return Err("gensymmed symbols are non-representable");
        }

        match glsp::parse_1(&self.name(), None).unwrap() {
            Val::Int(_) | Val::Flo(_) => {
                Err("symbols which resemble numbers are non-representable")
            }
            Val::Arr(_) => Err("symbols which resemble abbreviations are non-representable"),
            Val::Sym(_) => Ok(()),
            _ => unreachable!(),
        }
    }

    ///Returns `true` if this symbol can be serialized and deserialized using Serde.
    ///
    ///This method is only present when the `"serde"` feature is enabled.
    #[cfg(feature = "serde")]
    pub fn is_serializable(&self) -> bool {
        self.check_serializability().is_ok()
    }

    ///Returns `Ok` if this symbol can be serialized and deserialized using Serde.
    ///
    ///This method is only present when the `"serde"` feature is enabled.
    #[cfg(feature = "serde")]
    pub fn check_serializability(&self) -> Result<(), &'static str> {
        Ok(())
    }
}

//-------------------------------------------------------------------------------------------------
// Display/Debug implementations
//-------------------------------------------------------------------------------------------------

//note that many of these implementations, e.g. for Sym, will panic if there is no active Engine.
//this is because these types should be either impossible, or very difficult, to access in the
//absence of an active Engine.

macro_rules! impl_forwarding_debug {
    ($($type:ty),+) => (
        $(
            //emit a Debug implementation which just forwards to this type's Display implementation
            impl Debug for $type {
                fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                    <Self as Display>::fmt(self, f)
                }
            }
        )+
    );
}

impl_forwarding_debug!(Arr, Tab, GIter, Sym, Obj, Class, GFn, RFn, Coro, RData);

// Root, Raw
//------------------------------

impl<T: Allocate + Display> Display for Root<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: Allocate + Debug> Debug for Root<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: Allocate> Pointer for Root<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Pointer::fmt(&(&**self as *const T), f)
    }
}

impl<T: Allocate + Display> Display for Raw<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: Allocate + Debug> Debug for Raw<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: Allocate> Pointer for Raw<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Pointer::fmt(&(&**self as *const T), f)
    }
}

// Slot, Val
//------------------------------

impl Display for Slot {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#}", self.into_val().unwrap())
        } else {
            write!(f, "{}", self.into_val().unwrap())
        }
    }
}

impl Debug for Slot {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", self.into_val().unwrap())
        } else {
            write!(f, "{:?}", self.into_val().unwrap())
        }
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Val::Nil => write!(f, "#n"),
            Val::Int(i) => write!(f, "{}", i),
            Val::Char(ch) => write!(f, "{}", ch),
            Val::Flo(flo) if flo == f32::INFINITY => write!(f, "+inf.0"),
            Val::Flo(flo) if flo == f32::NEG_INFINITY => write!(f, "-inf.0"),
            Val::Flo(flo) if flo.is_nan() => write!(f, "nan.0"),
            Val::Flo(flo) => {
                use std::io::Write;

                //there's no easy way to guarantee a minimum number of decimal places using
                //write!(), but we don't want 42.0 to be printed as "42"!
                let mut buf = SmallVec::<[u8; 64]>::new();
                write!(&mut buf, "{:?}", flo).unwrap();

                if !buf.contains(&b'.') && !buf.contains(&b'e') && !buf.contains(&b'E') {
                    buf.extend_from_slice(&[b'.', b'0']);
                }

                write!(f, "{}", str::from_utf8(&buf[..]).unwrap())
            }
            Val::Bool(b) => write!(f, "{}", if b { "#t" } else { "#f" }),
            Val::Sym(s) => write!(f, "{}", s),
            Val::Arr(ref root) => {
                if f.alternate() {
                    write!(f, "{:#}", root)
                } else {
                    write!(f, "{}", root)
                }
            }
            Val::Str(ref root) => write!(f, "{}", root),
            Val::Tab(ref root) => {
                if f.alternate() {
                    write!(f, "{:#}", root)
                } else {
                    write!(f, "{}", root)
                }
            }
            Val::GIter(ref root) => write!(f, "{}", root),
            Val::Obj(ref root) => write!(f, "{}", root),
            Val::Class(ref root) => write!(f, "{}", root),
            Val::GFn(ref root) => write!(f, "{}", root),
            Val::Coro(ref root) => write!(f, "{}", root),
            Val::RData(ref root) => write!(f, "{}", root),
            Val::RFn(ref root) => write!(f, "{}", root),
        }
    }
}

impl Debug for Val {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Val::Str(ref root) => write!(f, "{:?}", root),
            Val::Char(ch) => match ch {
                ' ' => write!(f, "\\space"),
                '\t' => write!(f, "\\tab"),
                '\n' => write!(f, "\\newline"),
                '\r' => write!(f, "\\return"),
                '\0' => write!(f, "\\nul"),
                ch if (ch as u32) < 32 => {
                    write!(
                        f,
                        "\\x{}{}",
                        char::from_digit((ch as u32) / 16, 16).unwrap(),
                        char::from_digit((ch as u32) % 16, 16).unwrap()
                    )
                }
                ch => write!(f, "\\{}", ch),
            },
            _ => <Self as Display>::fmt(self, f),
        }
    }
}

// atoms (i.e. types which don't need to be handled differently when pretty-printing)
//------------------------------

impl Display for Sym {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", &self.name())
    }
}

impl Display for RFn {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.name.get() {
            Some(name) => write!(f, "#<rfn:{}>", name),
            None => write!(f, "#<rfn>"),
        }
    }
}

impl Display for Str {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", &self.to_rust_string())
    }
}

impl Debug for Str {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "\"{}\"", &self.to_escaped_string())
    }
}

impl Display for GIter {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "#<iter:{}>", self.state_name())
    }
}

impl Display for Obj {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.class().name() {
            Some(name) => write!(f, "#<obj:{}>", name),
            None => write!(f, "#<obj>"),
        }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match (self.name(), self.is_mixin()) {
            (Some(name), true) => write!(f, "#<mixin:{}>", name),
            (Some(name), false) => write!(f, "#<class:{}>", name),
            (None, true) => write!(f, "#<mixin>"),
            (None, false) => write!(f, "#<class>"),
        }
    }
}

impl Display for GFn {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.lambda.name {
            Some(name) => write!(f, "#<fn:{}>", name),
            None => write!(f, "#<fn>"),
        }
    }
}

impl Display for Coro {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.gfn().lambda.name {
            Some(name) => write!(f, "#<coro:{}>", name),
            None => write!(f, "#<coro>"),
        }
    }
}

impl Display for RData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(rclass) = self.rclass.as_ref() {
            write!(f, "#<rdata:{}>", rclass.name())
        } else {
            write!(f, "#<rdata>")
        }
    }
}

// Arr, Tab
//------------------------------

impl Display for Arr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            let text = PrettyPrinter::new().print_arr(self);
            write!(f, "{}", text)
        } else {
            let mut parents = SmallVec::<[usize; 64]>::new();
            ugly_print_arr(self, f, &mut parents)
        }
    }
}

impl Display for Tab {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            let text = PrettyPrinter::new().print_tab(self);
            write!(f, "{}", text)
        } else {
            let mut parents = SmallVec::<[usize; 64]>::new();
            ugly_print_tab(self, f, &mut parents)
        }
    }
}

//-------------------------------------------------------------------------------------------------
// the ugly-printer
//-------------------------------------------------------------------------------------------------

fn ugly_print_val(
    val: &Val,
    f: &mut Formatter,
    parents: &mut SmallVec<[usize; 64]>,
) -> fmt::Result {
    match val {
        Val::Arr(ref arr) => ugly_print_arr(arr, f, parents),
        Val::Tab(ref tab) => ugly_print_tab(tab, f, parents),
        _ => write!(f, "{:?}", val),
    }
}

fn ugly_print_arr(
    arr: &Arr,
    f: &mut Formatter,
    parents: &mut SmallVec<[usize; 64]>,
) -> fmt::Result {
    let address = arr as *const Arr as usize;
    for (i, parent_address) in parents.iter().rev().enumerate() {
        if *parent_address == address {
            return write!(f, "#<cycle:{}>", i);
        }
    }

    parents.push(address);

    if let Some((abbrv, form)) = detect_abbrv(arr) {
        write!(f, "{}{}", abbrv, form)?;
    } else if arr.len() >= 1 && arr.get::<Val>(0).unwrap() == Val::Sym(TEMPLATE_STR_SYM) {
        write!(f, "\"")?;

        for val in arr.iter().skip(1) {
            match val {
                Val::Str(st) => write!(f, "{}", st.to_escaped_string())?,
                _ => {
                    write!(f, "{{")?;
                    ugly_print_val(&val, f, parents)?;
                    write!(f, "}}")?;
                }
            }
        }

        write!(f, "\"")?;
    } else {
        let is_access = arr.len() >= 1 && arr.get::<Val>(0).unwrap() == Val::Sym(ACCESS_SYM);

        write!(f, "{}", if is_access { "[" } else { "(" })?;

        for (i, val) in arr.iter().skip(if is_access { 1 } else { 0 }).enumerate() {
            if i != 0 {
                write!(f, " ")?;
            }
            ugly_print_val(&val, f, parents)?;
        }

        write!(f, "{}", if is_access { "]" } else { ")" })?;
    }

    parents.pop().unwrap();
    Ok(())
}

fn ugly_print_tab(
    tab: &Tab,
    f: &mut Formatter,
    parents: &mut SmallVec<[usize; 64]>,
) -> fmt::Result {
    let address = tab as *const Tab as usize;
    for (i, parent_address) in parents.iter().rev().enumerate() {
        if *parent_address == address {
            return write!(f, "#<cycle:{}>", i);
        }
    }

    parents.push(address);

    write!(f, "#(")?;
    for (i, (key, value)) in tab.entries().iter().enumerate() {
        if i != 0 {
            write!(f, " ")?;
        }

        write!(f, "(")?;
        ugly_print_val(&key, f, parents)?;
        write!(f, " ")?;
        ugly_print_val(&value, f, parents)?;
        write!(f, ")")?;
    }
    write!(f, ")")?;

    parents.pop().unwrap();
    Ok(())
}

fn detect_abbrv(arr: &Arr) -> Option<(&'static str, Val)> {
    if arr.len() == 2 {
        let form = arr.get::<Val>(1).unwrap();

        match arr.get::<Val>(0).unwrap() {
            Val::Sym(QUOTE_SYM) => Some(("'", form)),
            Val::Sym(BACKQUOTE_SYM) => Some(("`", form)),
            Val::Sym(UNQUOTE_SYM) => Some(("~", form)),
            Val::Sym(SPLAY_SYM) => Some(("..", form)),
            Val::Sym(MET_NAME_SYM) => Some((".", form)),
            Val::Sym(ATSIGN_SYM) => Some(("@", form)),
            _ => None,
        }
    } else {
        None
    }
}

//-------------------------------------------------------------------------------------------------
// the pretty-printer
//-------------------------------------------------------------------------------------------------

/*

the current pretty-printing rules are designed to be very simple to implement, and produce
results which are "readable enough" for e.g. basic debugging of expanded macro output.

all values except arrs and tabs are stringified using Debug, and their results are treated as
indivisible. this means that large string literals will overshoot the column limit.

when arrs begin with certain "breaking" syms, like `if` or `defn`, they try to emit a certain
number of arguments on the same line as their opening delimeter, then emit every subsequent
argument on its own line.

otherwise, arrs and tabs pretty-stringify each of their arguments separately. if any of the
arguments recursively contain a breaking form, we rewind to the start of the arr and stack each
argument one per line like a `do`. otherwise, we just emit arguments one after the other,
line-breaking where necessary.

*/

const LINE_LIMIT: usize = 100;
const MIN_LINE_WIDTH: usize = 75;
const INDENT_INCREMENT: usize = 2;

//we could use stock syms here, but it ends up clogging up the stock syms database pretty badly,
//in exchange for not-very-much performance gain.
static BREAKING_SYMS: [(&str, usize); 48] = [
    ("do", 1),
    ("do-0", 1),
    ("cond", 1),
    ("match", 1),
    ("tab", 1),
    ("class", 1),
    ("mixin", 1),
    ("fsm", 1),
    ("loop", 1),
    ("defer", 1),
    ("defer-yield", 1),
    ("prop", 1),
    ("wrap-prop", 1),
    ("fini", 1),
    ("fini-state", 1),
    ("fini-mixin", 1),
    ("try", 1),
    ("try-verbose", 1),
    ("if", 2),
    ("block", 2),
    ("fn", 2),
    ("when", 2),
    ("unless", 2),
    ("while", 2),
    ("until", 2),
    ("cond==", 2),
    ("cond-same?", 2),
    ("cond-eq?", 2),
    ("state", 2),
    ("state*", 2),
    ("init", 2),
    ("init-state", 2),
    ("init-mixin", 2),
    ("defclass", 2),
    ("let-class", 2),
    ("defmixin", 2),
    ("let-mixin", 2),
    ("defstruct", 2),
    ("defmacro", 3),
    ("defn", 3),
    ("let-macro", 3),
    ("let-fn", 3),
    ("met", 3),
    ("wrap", 3),
    ("when-let", 3),
    ("for", 4),
    ("forn", 4),
    ("forni", 4),
];

struct PrettyPrinter {
    breaking_syms: HashMap<Sym, usize>,
    indent: usize,
    cursor_x: usize,

    builder: String,
    parents: Vec<usize>,
}

impl PrettyPrinter {
    fn new() -> PrettyPrinter {
        let breaking_syms: HashMap<_, _> = BREAKING_SYMS
            .iter()
            .map(|&(st, n)| (glsp::sym(st).unwrap(), n))
            .collect();

        PrettyPrinter {
            breaking_syms,
            indent: 0,
            cursor_x: 0,

            builder: String::new(),
            parents: Vec::new(),
        }
    }

    fn print_arr(mut self, arr: &Arr) -> String {
        self.recursively_build_arr(arr);
        self.builder
    }

    fn print_tab(mut self, tab: &Tab) -> String {
        self.recursively_build_tab(tab);
        self.builder
    }

    //returns `true` if this val or any of its children contained at least one breaking form.
    fn recursively_build_val(&mut self, arg: &Val) -> bool {
        match *arg {
            Val::Arr(ref arr) => self.recursively_build_arr(arr),
            Val::Tab(ref tab) => self.recursively_build_tab(tab),
            _ => {
                self.push_str(&format!("{:?}", arg));
                false
            }
        }
    }

    fn recursively_build_arr(&mut self, arr: &Arr) -> bool {
        //check for cycles
        let address = arr as &Arr as *const Arr as usize;

        for (i, parent) in self.parents.iter().rev().enumerate() {
            if *parent == address {
                self.push_str(&format!("#<cycle:{}>", i));
                return false;
            }
        }

        //check for abbreviations. they get a tail-call return, since we consider them to be part
        //of the form which immediately follows them
        if let Some((abbrv, val)) = detect_abbrv(arr) {
            self.push_str(abbrv);

            self.parents.push(address);
            let result = self.recursively_build_val(&val);
            self.parents.pop().unwrap();

            return result;
        }

        //the (access) abbreviation, [], has special handling
        let is_access = arr.len() >= 1 && arr.get::<Val>(0).unwrap() == Val::Sym(ACCESS_SYM);

        //delegate to recursively_build_sequence()
        self.parents.push(address);

        self.push_str(if is_access { "[" } else { "(" });
        let sequence: SmallVec<[Val; 8]> = arr.iter().collect();
        let elems = if is_access {
            &sequence[1..]
        } else {
            &sequence[..]
        };
        let result = self.recursively_build_sequence(elems, false);
        self.push_str(if is_access { "]" } else { ")" });

        self.parents.pop().unwrap();

        result
    }

    fn recursively_build_tab(&mut self, tab: &Tab) -> bool {
        //check for cycles
        let address = tab as &Tab as *const Tab as usize;

        for (i, parent) in self.parents.iter().rev().enumerate() {
            if *parent == address {
                self.push_str(&format!("#<cycle:{}>", i));
                return false;
            }
        }

        //construct the sequence
        let mut sequence = SmallVec::<[Val; 8]>::with_capacity(tab.len() * 2);
        for (key, value) in tab.entries().iter() {
            sequence.push(key);
            sequence.push(value);
        }

        //delegate to recursively_build_sequence()
        self.parents.push(address);

        self.push_str("#(");
        let result = self.recursively_build_sequence(&sequence[..], true);
        self.push_str(")");

        self.parents.pop().unwrap();

        result
    }

    //we convert both arrs and tabs into a uniform format: an array of vals and an "is a tab"
    //flag. tabs stringify two vals at a time, arrs stringify each val individually.
    fn recursively_build_sequence(&mut self, sequence: &[Val], is_tab: bool) -> bool {
        //we start out assuming that we can print this form in a non-breaking fashion. if we're
        //proven wrong, we "rewind" to the start of the form and try again.
        let start_len = self.builder.len();
        let start_cursor_x = self.cursor_x;
        let mut max_len = self.builder.len() + self.column_limit().saturating_sub(self.cursor_x);

        let mut is_breaking = false;

        if let Some(&Val::Sym(first)) = sequence.get(0) {
            if self.breaking_syms.get(&first).is_some() {
                is_breaking = true;
            }
        }

        let mut i = 0;
        self.indent += INDENT_INCREMENT;
        while i < sequence.len() {
            if is_breaking {
                break;
            }

            if i != 0 {
                self.push_str(" ");
            }

            if self.builder.len() >= max_len && i != 0 {
                self.push_newline();
                max_len = self.builder.len() + self.column_limit().saturating_sub(self.cursor_x);
            }

            if is_tab {
                self.push_str("(");
                is_breaking |= self.recursively_build_val(&sequence[i]);
                self.push_str(" ");
                is_breaking |= self.recursively_build_val(&sequence[i + 1]);
                self.push_str(")");
                i += 2;
            } else {
                is_breaking |= self.recursively_build_val(&sequence[i]);
                i += 1;
            }
        }
        self.indent -= INDENT_INCREMENT;

        if is_breaking {
            //at least one argument was breaking. we rewind...
            self.builder.truncate(start_len);
            self.cursor_x = start_cursor_x;

            //...and print again, with linebreaks between each argument
            let mut break_count = 0;
            if let Some(&Val::Sym(first)) = sequence.get(0) {
                break_count = self.breaking_syms.get(&first).cloned().unwrap_or(0);
            }

            let mut i = 0;
            self.indent += INDENT_INCREMENT;
            while i < sequence.len() {
                if is_tab {
                    self.push_str("(");
                    self.recursively_build_val(&sequence[i]);
                    self.push_str(" ");
                    self.recursively_build_val(&sequence[i + 1]);
                    self.push_str(")");
                    self.push_newline();
                    i += 2;
                } else {
                    if i != 0 && i < break_count {
                        self.push_str(" ");
                    }
                    self.recursively_build_val(&sequence[i]);
                    i += 1;
                    if i >= break_count && i != sequence.len() {
                        self.push_newline();
                    }
                }
            }
            self.indent -= INDENT_INCREMENT;
        }

        is_breaking
    }

    fn column_limit(&self) -> usize {
        max(LINE_LIMIT, self.indent + MIN_LINE_WIDTH)
    }

    fn push_str(&mut self, st: &str) {
        self.builder.push_str(st);
        self.cursor_x += st.len();
    }

    fn push_newline(&mut self) {
        self.builder.push('\n');
        self.builder.extend(repeat(' ').take(self.indent));
        self.cursor_x = self.indent;
    }
}

/*

if i ever find myself with a couple of days free, the improved pretty-printing rules will be:

- the following forms always linebreak every argument and specify no other rules: do, class.
    - when the immediate child of a class, defclass, state or state* form, these calls follow the
      same rules as do: fsm.
    - when the immediate child of a bind or def form: fn*
- these forms always start linebreaking after their first argument: if, block, when,
  unless, while, until, do, dotab, defclass.
    - when the immediate child of a class, defclass or fsm form: state, state*
    - when the arguments to `if` are very short (< 20 chars?), it will not linebreak, and instead
      acts like an indivisible item. (should this rule be generalised to all forms?)
- these forms follow "function rules": defn, defmacro, let-fn (n=2)
    - when the immediate child of a class, defclass, state or state* form: met, wrap (n=2)
    - when the immediate child of a def form: fn (n=1)
    - "function rules" are to always start linebreaking after the nth argument, and to have
      special handling for the parameter list, indenting linebreaks to its opening delimeter.
- these forms follow "for rules": for, forn, forni
    - start linebreaking after first argument. if the first argument is an arr of arrs, linebreak
      between every element and indent to the starting delimiter.
- these forms follow "cond rules": cond, cond==, cond-same?, cond-eq?
    - linebreak every argument. for each argument which is an arr, start linebreaking after the
      arr's first element, with an extra indentation level.
- some forms always have extra linebreaks inserted before/after them:
- tab, and tab literals, follow "table rules". if they fit within a single line, no change.
  if not, then follow "cond rules".
- every other arr is assumed to follow generic "call rules". if at least one argument is
  multi-line, or if the total length would exceed the column limit, then it falls back to do
  rules. otherwise, it's emitted as an indivisible unit.

- the implementation should definitely build an intermediate representation... the "rewind"
  algorithm used above is a complete mess in comparison, and any perf cost will probably pale
  in comparison to the cost of emitting the pretty-printed data to console or writing it to disk.
- the column limit is always at least 75 more than the current indentation level (although this,
  along with the column limit itself, should be customisable).
- `template-str` forms should be converted to actual template strs, with the embedded glsp code
  being pretty-printed where appropriate.
- long strs should be divided into multi-line "foo \\n bar" strings if necessary. the pretty
  printer should make a best-effort attempt to linebreak strings at whitespace.
- the user should be able to add their own rules to the pretty-printer so that their own macros
  will be pretty-printed. probably simplest if this is a rust api rather than a glsp one.
    - an adequate api would probably be "have the pretty-printer treat this symbol as though
      it's actually something else", i.e. treat `def-my-special-class` as `defclass`.

*/

/*
enum Rule {
    CallRule,
    DoRule,
    ClassRule(usize),
    FunctionRule,
    ParamsRule,
    WhenRule,
    ForRule,
    TabRule(usize),
    CondRule,
    CondClauseRule
}

static KNOWN_SYMS: [(&'static str, Rule); _] = [
    ("do", DoRule),
    ("class", ClassRule(0)),
    ("defclass", ClassRule(1)),
    ("state", ClassRule(1)),
    ("state*", ClassRule(1)),
    ...
];
*/

//-------------------------------------------------------------------------------------------------
// printing ParamMap, Bytecode and Instr
//-------------------------------------------------------------------------------------------------

pub(crate) fn param_map_to_string(param_map: &ParamMap) -> String {
    let mut builder = String::new();

    builder.push_str(&format!(
        "\tbasic params: {}\n",
        param_map.basic_param_count
    ));
    builder.push_str(&format!("\t(?) params:   {}\n", param_map.opt_param_count));
    builder.push_str(&format!(
        "\t.. param:     {}",
        if param_map.rest_param.is_some() {
            "yes"
        } else {
            "no"
        }
    ));

    builder
}

pub(crate) fn bytecode_to_string(code: &Bytecode) -> String {
    let mut builder = String::new();

    builder.push_str(&format!(
        "\tregisters:    {} ({} locals, {} scratch, {} literals)",
        code.start_regs.len(),
        code.local_count,
        code.scratch_count,
        code.literal_count
    ));

    builder.push_str(&format!("\n\tstays:        {}\n", code.start_stays.len()));

    builder.push_str("\n\tbytecode:");
    for instr in &code.instrs {
        let instr_str = instr_to_string(*instr, code.local_count, code.scratch_count);
        builder.push_str(&format!("\n\t\t{}", instr_str));
    }

    if code.literal_count + code.local_count > 0 {
        builder.push_str("\n\n\tliterals:");
    }

    let literal_start = code.local_count + code.scratch_count;
    for (n, i) in (0u8..code.local_count)
        .enumerate()
        .chain((literal_start..literal_start + code.literal_count).enumerate())
    {
        let val_str = format!("{:?}", &code.start_regs[i as usize].root());
        let base_str = if i >= literal_start { "lit" } else { "loc" };

        if i == literal_start && code.local_count > 0 {
            builder.push('\n');
        }

        if val_str.len() > 60 {
            let msg = format!("\n\t\t{}{} = {}...", base_str, n, &val_str[..56]);
            builder.push_str(&msg);
        } else {
            let msg = format!("\n\t\t{}{} = {}", base_str, n, &val_str);
            builder.push_str(&msg);
        }
    }

    if !code.start_stays.is_empty() {
        builder.push_str("\n\n\tstays:\n");

        for (i, stay_source) in code.start_stays.iter().enumerate() {
            builder.push_str(&format!("\n\t\tstay_id {} = {:?}", i, stay_source));
        }
    }

    /*builder.push_str("\n\n\tspans:");
    for span in &code.spans {
        builder.push_str(&format!("\n\t\t{:?}", span));
    }*/

    builder
}

pub(crate) fn instr_to_string(instr: Instr, local_count: u8, scratch_count: u8) -> String {
    use Instr::*;

    let reg = move |reg_id: u8| -> String {
        if reg_id < local_count {
            format!("loc{}", reg_id)
        } else if reg_id < local_count + scratch_count {
            format!("scr{}", reg_id - local_count)
        } else {
            format!("lit{}", reg_id - (local_count + scratch_count))
        }
    };

    match instr {
        CopyRegister(dst, src) => {
            format!("CopyRegister(dst_reg: {}, src_reg: {})", reg(dst), reg(src))
        }
        LoadGlobal(dst, sym_bytes) => {
            format!(
                "LoadGlobal(dst_reg: {}, sym: {})",
                reg(dst),
                Sym::from(sym_bytes)
            )
        }
        SetGlobal(src, sym_bytes) => {
            format!(
                "SetGlobal(src_reg: {}, sym: {})",
                reg(src),
                Sym::from(sym_bytes)
            )
        }
        LoadStay(dst, stay_id) => {
            format!("LoadStay(dst_reg: {}, stay_id: {})", reg(dst), stay_id)
        }
        SetStay(src, stay_id) => {
            format!("SetStay(src_reg: {}, stay_id: {})", reg(src), stay_id)
        }
        MakeStay(src, stay_id) => {
            format!("MakeStay(src_reg: {}, stay_id: {})", reg(src), stay_id)
        }
        MakeGFn(dst, lambda_id) => {
            format!("MakeGFn(dst_reg: {}, lambda_id: {})", reg(dst), lambda_id)
        }
        Call0(dst, callee) => {
            format!("Call0(dst_reg: {}, callee_reg: {})", reg(dst), reg(callee))
        }
        Call1(dst, callee, arg) => {
            format!(
                "Call1(dst_reg: {}, callee_reg: {}, arg_reg: {})",
                reg(dst),
                reg(callee),
                reg(arg)
            )
        }
        Call2(dst, callee, arg0, arg1) => {
            format!(
                "Call2(dst_reg: {}, callee_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(callee),
                reg(arg0),
                reg(arg1)
            )
        }
        CallN(dst, base, arg_count) => {
            format!(
                "CallN(dst_reg: {}, base_reg: {}, arg_count: {})",
                reg(dst),
                reg(base),
                arg_count
            )
        }
        Splay(bits) => {
            format!("Splay(bits: {:?})", bits)
        }
        Return(src) => {
            format!("Return(src_reg: {})", reg(src))
        }
        Yield(dst, result) => {
            format!("Yield(dst_reg: {}, result_reg: {})", reg(dst), reg(result))
        }
        Jump(jump_bytes) => {
            format!("Jump(jump: {})", isize::from(jump_bytes))
        }
        JumpIfTrue(src, jump_bytes) => {
            format!(
                "JumpIfTrue(src_reg: {}, jump: {})",
                reg(src),
                isize::from(jump_bytes)
            )
        }
        JumpIfFalse(src, jump_bytes) => {
            format!(
                "JumpIfFalse(src_reg: {}, jump: {})",
                reg(src),
                isize::from(jump_bytes)
            )
        }
        PushDefer(defer) => {
            format!("PushDefer(defer: {})", defer)
        }
        RunAndPopDefers(defer_count) => {
            format!("RunAndPopDefers(defer_count: {})", defer_count)
        }
        RunDefer(defer) => {
            format!("RunDefer(defer: {})", defer)
        }
        EndDefer() => {
            format!("EndDefer()")
        }
        OpAdd(dst, arg0, arg1) => {
            format!(
                "OpAdd(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpSub(dst, arg0, arg1) => {
            format!(
                "OpSub(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpMul(dst, arg0, arg1) => {
            format!(
                "OpMul(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpDiv(dst, arg0, arg1) => {
            format!(
                "OpDiv(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpRem(dst, arg0, arg1) => {
            format!(
                "OpRem(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpAbs(dst, arg) => {
            format!("OpAbs(dst_reg: {}, arg_reg: {})", reg(dst), reg(arg))
        }
        OpNeg(dst, arg) => {
            format!("OpNeg(dst_reg: {}, arg_reg: {})", reg(dst), reg(arg))
        }
        OpSign(dst, arg) => {
            format!("OpSign(dst_reg: {}, arg_reg: {})", reg(dst), reg(arg))
        }
        OpMin(dst, arg0, arg1) => {
            format!(
                "OpMin(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpMax(dst, arg0, arg1) => {
            format!(
                "OpMax(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpPredicate(dst, arg, predicate) => {
            format!(
                "OpPredicate(dst_reg: {}, arg_reg: {}, predicate: {:?})",
                reg(dst),
                reg(arg),
                predicate
            )
        }
        OpInt(dst, arg) => {
            format!("OpInt(dst_reg: {}, arg_reg: {})", reg(dst), reg(arg))
        }
        OpFlo(dst, arg) => {
            format!("OpFlo(dst_reg: {}, arg_reg: {})", reg(dst), reg(arg))
        }
        OpBool(dst, arg) => {
            format!("OpBool(dst_reg: {}, arg_reg: {})", reg(dst), reg(arg))
        }
        OpNumEq(dst, arg0, arg1) => {
            format!(
                "OpNumEq(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpLt(dst, arg0, arg1) => {
            format!(
                "OpLt(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpLte(dst, arg0, arg1) => {
            format!(
                "OpLte(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpGt(dst, arg0, arg1) => {
            format!(
                "OpGt(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpGte(dst, arg0, arg1) => {
            format!(
                "OpGte(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpNot(dst, arg) => {
            format!("OpNot(dst_reg: {}, arg_reg: {})", reg(dst), reg(arg))
        }
        OpIter(dst, arg) => {
            format!("OpIter(dst_reg: {}, arg_reg: {})", reg(dst), reg(arg))
        }
        OpIterNext(dst, arg) => {
            format!("OpIterNext(dst_reg: {}, arg_reg: {})", reg(dst), reg(arg))
        }
        OpIterNextBack(dst, arg) => {
            format!(
                "OpIterNextBack(dst_reg: {}, arg_reg: {})",
                reg(dst),
                reg(arg)
            )
        }
        OpIterFinishedp(dst, arg) => {
            format!(
                "OpIterFinishedp(dst_reg: {}, arg_reg: {})",
                reg(dst),
                reg(arg)
            )
        }
        OpAccess(dst, arg0, arg1) => {
            format!(
                "OpAccess(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpSetAccess(dst, arg0, arg1, arg2) => {
            format!(
                "OpSetAccess(dst_reg: {}, arg0_reg: {}, arg1_reg: {}, arg2_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1),
                reg(arg2)
            )
        }
        OpLen(dst, arg) => {
            format!("OpLen(dst_reg: {}, arg_reg: {})", reg(dst), reg(arg))
        }
        OpHasp(dst, arg0, arg1) => {
            format!(
                "OpHasp(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
        OpArr(dst, arg0, arg_count) => {
            format!(
                "OpArr(dst_reg: {}, arg0_reg: {}, arg_count: {})",
                reg(dst),
                reg(arg0),
                arg_count
            )
        }
        OpCallMet(dst, arg0, arg_count) => {
            format!(
                "OpCallMet(dst_reg: {}, arg0_reg: {}, arg_count: {})",
                reg(dst),
                reg(arg0),
                arg_count
            )
        }
        OpCallMetOpt(dst, arg0, arg_count) => {
            format!(
                "OpCallMetOpt(dst_reg: {}, arg0_reg: {}, arg_count: {})",
                reg(dst),
                reg(arg0),
                arg_count
            )
        }
        OpCallBaseRaw(dst, arg0, arg_count) => {
            format!(
                "OpCallBaseRaw(dst_reg: {}, arg0_reg: {}, arg_count: {})",
                reg(dst),
                reg(arg0),
                arg_count
            )
        }
        OpGlobal(dst, arg) => {
            format!("OpGlobal(dst_reg: {}, arg_reg: {})", reg(dst), reg(arg))
        }
        OpSetGlobal(dst, arg0, arg1) => {
            format!(
                "OpSetGlobal(dst_reg: {}, arg0_reg: {}, arg1_reg: {})",
                reg(dst),
                reg(arg0),
                reg(arg1)
            )
        }
    }
}

#[doc(hidden)]
pub fn dump_form(arg: &Val) -> GResult<String> {
    let expanded = glsp::expand(arg, None)?;

    let toplevel_lets = HashMap::new();

    let mut ast = Ast::new();
    let node = ast.node_from_val(&expanded, Span::default())?;
    transform::standard_passes(&mut ast, node);
    let code = encoder::encode_fragment(&ast, node, &toplevel_lets)?;

    Ok(format!(
        "ANONYMOUS FORM:\n\n{}\n",
        bytecode_to_string(&code)
    ))
}

#[doc(hidden)]
pub fn dump_fn(gfn: &GFn) -> GResult<String> {
    let name = match gfn.lambda.name {
        Some(name) => format!("FN: {}", name),
        _ => "ANONYMOUS FN:".to_string(),
    };

    let params_str = param_map_to_string(&gfn.lambda.param_map);
    let code_str = bytecode_to_string(&gfn.lambda.bytecode);

    Ok(format!("{}\n\n{}\n{}\n", name, params_str, code_str))
}

#[doc(hidden)]
pub fn dump_macro(sym: Sym) -> GResult<String> {
    let gfn = match glsp::get_macro(sym) {
        Ok(Expander::GFn(gfn)) => gfn,
        _ => bail!("the name '{}' is not bound to a gfn macro", sym),
    };

    let params_str = param_map_to_string(&gfn.lambda.param_map);
    let code_str = bytecode_to_string(&gfn.lambda.bytecode);

    Ok(format!(
        "MACRO: {}\n\n{}\n\n{}\n",
        sym, params_str, code_str
    ))
}
