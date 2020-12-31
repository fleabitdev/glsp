#![cfg(feature = "serde")]

use serde::de::{
	Deserialize, Deserializer, Error as DeError, EnumAccess, MapAccess, 
	SeqAccess, VariantAccess, Visitor
};
use serde::ser::{Error as SerError, Serialize, Serializer, SerializeMap, SerializeSeq};
use std::{fmt};
use std::rc::{Rc};
use super::collections::{Arr, DequeOps, Str, Tab};
use super::engine::{glsp, Sym};
use super::gc::{Allocate, Raw, Slot, Root};
use super::val::{Val};

/*

this module is only present when the "serde" crate feature is enabled. it contains manual
Serialize and Deserialize implementations for various glsp types.

it pulls in the `serde` crate as a dependency, but not `serde_derive` (because that would pull
in `proc_macro2`, `quote` and `syn`). we do depend on `serde_derive` if the "compiler" feature is
enabled, to make it easier to define Serialize/Deserialize for Bytecode, Lambda, etc.

*/

//-------------------------------------------------------------------------------------------------
// Serialize
//-------------------------------------------------------------------------------------------------

impl Serialize for Val {
	fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
		if let Err(err) = self.check_serializability() {
			return Err(S::Error::custom(format!("serialization error: {}", err)))
		}

		Unchecked(self).serialize(s)
	}
}

impl Serialize for Slot {
	fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
		self.root().serialize(s)
	}
}

impl Serialize for Sym {
	fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
		if let Err(err) = self.check_serializability() {
			return Err(S::Error::custom(format!("serialization error: {}", err)))
		}

		let rc: Rc<str> = self.name();
		s.serialize_str(&*rc)
	}
}

impl Serialize for Arr {
	fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
		if let Err(err) = self.check_serializability() {
			return Err(S::Error::custom(format!("serialization error: {}", err)))
		}

		let mut seq = s.serialize_seq(Some(self.len()))?;
		for val in self.iter() {
			seq.serialize_element(&val)?;
		}
		seq.end()
	}
}

impl Serialize for Str {
	fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
		s.serialize_str(&format!("{}", self))
	}
}

impl Serialize for Tab {
	fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
		if let Err(err) = self.check_serializability() {
			return Err(S::Error::custom(format!("serialization error: {}", err)))
		}

		let mut map = s.serialize_map(Some(self.len()))?;
		for (key, value) in self.entries().iter() {
			map.serialize_entry(&key, &value)?;
		}
		map.end()
	}
}

impl<T: Allocate + Serialize> Serialize for Root<T> {
	fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
		(**self).serialize(s)
	}
}

impl<T: Allocate + Serialize> Serialize for Raw<T> {
	fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
		self.root().serialize(s)
	}
}

//we consider the Serialize implementations above to be the "public-facing api" to the
//serializers for the Unchecked wrapper type. the public-facing implementations check that the
//input doesn't contain any non-representable data, then recursively serialize their input without 
//needing to repeat those checks.
struct Unchecked<T>(T);

impl<'a> Serialize for Unchecked<&'a Val> {
	fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
		match *self.0 {
			Val::Nil => s.serialize_unit_variant("Val", 0, "Val::Nil"),
			Val::Int(i) => s.serialize_newtype_variant("Val", 1, "Val::Int", &i),
			Val::Flo(f) => s.serialize_newtype_variant("Val", 2, "Val::Flo", &f),
			Val::Char(c) => s.serialize_newtype_variant("Val", 3, "Val::Char", &c),
			Val::Bool(b) => s.serialize_newtype_variant("Val", 4, "Val::Bool", &b),
			Val::Sym(sym) => s.serialize_newtype_variant("Val", 5, "Val::Sym", &sym),
			Val::Arr(ref a) => s.serialize_newtype_variant("Val", 6, "Val::Arr", &Unchecked(&**a)),
			Val::Str(ref st) => s.serialize_newtype_variant("Val", 7, "Val::Str", &**st),
			Val::Tab(ref t) => s.serialize_newtype_variant("Val", 8, "Val::Tab", &Unchecked(&**t)),
			_ => unreachable!()
		}
	}
}

impl<'a> Serialize for Unchecked<&'a Arr> {
	fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
		let mut seq = s.serialize_seq(Some(self.0.len()))?;
		for val in self.0.iter() {
			seq.serialize_element(&Unchecked(&val))?;
		}
		seq.end()
	}
}

impl<'a> Serialize for Unchecked<&'a Tab> {
	fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
		let mut map = s.serialize_map(Some(self.0.len()))?;
		for (key, value) in self.0.entries().iter() {
			map.serialize_entry(&Unchecked(&key), &Unchecked(&value))?;
		}
		map.end()
	}
}


//-------------------------------------------------------------------------------------------------
// Deserialize
//-------------------------------------------------------------------------------------------------

enum ValVariant {
	Nil,
	Int,
	Flo,
	Char,
	Bool,
	Sym,
	Arr,
	Str,
	Tab
}

struct ValVariantVisitor;

impl<'de> Visitor<'de> for ValVariantVisitor {
	type Value = ValVariant;

	fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "a Val variant")
	}

	fn visit_u64<E: DeError>(self, v: u64) -> Result<Self::Value, E> {
		Ok(match v {
			0 => ValVariant::Nil,
			1 => ValVariant::Int,
			2 => ValVariant::Flo,
			3 => ValVariant::Char,
			4 => ValVariant::Bool,
			5 => ValVariant::Sym,
			6 => ValVariant::Arr,
			7 => ValVariant::Str,
			8 => ValVariant::Tab,
			_ => return Err(E::custom("invalid Val variant"))
		})
	}

	fn visit_str<E: DeError>(self, s: &str) -> Result<Self::Value, E> {
		Ok(match s {
			"Val::Nil" => ValVariant::Nil,
			"Val::Int" => ValVariant::Int,
			"Val::Flo" => ValVariant::Flo,
			"Val::Char" => ValVariant::Char,
			"Val::Bool" => ValVariant::Bool,
			"Val::Sym" => ValVariant::Sym,
			"Val::Arr" => ValVariant::Arr,
			"Val::Str" => ValVariant::Str,
			"Val::Tab" => ValVariant::Tab,
			_ => return Err(E::custom("invalid Val variant"))
		})
	}

	fn visit_bytes<E: DeError>(self, b: &[u8]) -> Result<Self::Value, E> {
		Ok(match b {
			b"Val::Nil" => ValVariant::Nil,
			b"Val::Int" => ValVariant::Int,
			b"Val::Flo" => ValVariant::Flo,
			b"Val::Char" => ValVariant::Char,
			b"Val::Bool" => ValVariant::Bool,
			b"Val::Sym" => ValVariant::Sym,
			b"Val::Arr" => ValVariant::Arr,
			b"Val::Str" => ValVariant::Str,
			b"Val::Tab" => ValVariant::Tab,
			_ => return Err(E::custom("invalid Val variant"))
		})
	}
}

impl<'de> Deserialize<'de> for ValVariant {
	fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
		d.deserialize_identifier(ValVariantVisitor)
	}
}

struct ValVisitor;

impl<'de> Visitor<'de> for ValVisitor {
	type Value = Val;

	fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "a Val")
	}

	fn visit_enum<A: EnumAccess<'de>>(self, a: A) -> Result<Self::Value, A::Error> {
		let val = match a.variant()? {
			(ValVariant::Nil, v) => { v.unit_variant()?; Val::Nil },
			(ValVariant::Int, v) => Val::Int(v.newtype_variant()?),
			(ValVariant::Flo, v) => Val::Flo(v.newtype_variant()?),
			(ValVariant::Char, v) => Val::Char(v.newtype_variant()?),
			(ValVariant::Bool, v) => Val::Bool(v.newtype_variant()?),
			(ValVariant::Sym, v) => Val::Sym(v.newtype_variant()?),
			(ValVariant::Arr, v) => Val::Arr(v.newtype_variant()?),
			(ValVariant::Str, v) => Val::Str(v.newtype_variant()?),
			(ValVariant::Tab, v) => Val::Tab(v.newtype_variant()?)
		};

		Ok(val)
	}
}

impl<'de> Deserialize<'de> for Val {
	fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
		const VARIANTS: &[&str] = &[
			"Val::Nil", "Val::Int", "Val::Flo", "Val::Char", "Val::Bool", "Val::Sym", 
			"Val::Arr", "Val::Str", "Val::Tab", "Val::Obj", "Val::Class", "Val::GFn",
			"Val::Coro", "Val::RData", "Val::RFn"
		];

		d.deserialize_enum("Val", VARIANTS, ValVisitor)
	}
}

struct SymVisitor;

impl<'de> Visitor<'de> for SymVisitor {
	type Value = Sym;

	fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "a Sym")
	}

	fn visit_str<E: DeError>(self, st: &str) -> Result<Self::Value, E> {
		match glsp::sym(st) {
			Ok(sym) => Ok(sym),
			Err(_) => return Err(E::custom(format!("invalid sym {}", st)))
		}
	}
}

impl<'de> Deserialize<'de> for Sym {
	fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
		d.deserialize_str(SymVisitor)
	}
}

struct RootArrVisitor;

impl<'de> Visitor<'de> for RootArrVisitor {
	type Value = Root<Arr>;

	fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "an Arr")
	}

	fn visit_seq<A: SeqAccess<'de>>(self, mut a: A) -> Result<Self::Value, A::Error> {
		let arr = match a.size_hint() {
			Some(len) => glsp::arr_with_capacity(len),
			None => glsp::arr()
		};

		while let Some(val) = a.next_element::<Val>()? {
			arr.push(val).unwrap();
		}

		Ok(arr)
	}
}

impl<'de> Deserialize<'de> for Root<Arr> {
	fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
		d.deserialize_seq(RootArrVisitor)
	}
}

struct RootStrVisitor;

impl<'de> Visitor<'de> for RootStrVisitor {
	type Value = Root<Str>;

	fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "a Str")
	}

	fn visit_str<E: DeError>(self, s: &str) -> Result<Self::Value, E> {
		Ok(glsp::str_from_rust_str(s))
	}
}

impl<'de> Deserialize<'de> for Root<Str> {
	fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
		d.deserialize_str(RootStrVisitor)
	}
}

struct RootTabVisitor;

impl<'de> Visitor<'de> for RootTabVisitor {
	type Value = Root<Tab>;

	fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "a Tab")
	}

	fn visit_map<A: MapAccess<'de>>(self, mut a: A) -> Result<Self::Value, A::Error> {
		let tab = match a.size_hint() {
			Some(len) => glsp::tab_with_capacity(len),
			None => glsp::tab()
		};

		while let Some((key, value)) = a.next_entry::<Val, Val>()? {
			tab.set(key, value).unwrap();
		}

		Ok(tab)
	}
}

impl<'de> Deserialize<'de> for Root<Tab> {
	fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
		d.deserialize_map(RootTabVisitor)
	}
}

