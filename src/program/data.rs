
use std::cell::{ RefCell, /*RefMut*/ };
use std::rc::{ Rc };
use std::fmt::{ Display, Formatter, Result as FmtResult };

use generational_arena::{ Arena, Index };

use crate::memory::{ EA };

// ------------------------------------------------------------------------------------------------
// Misc
// ------------------------------------------------------------------------------------------------

/// Type alias for `Rc<RefCell<T>>`.
pub type RcCell<T> = Rc<RefCell<T>>;

/// Shorthand or `Rc::new(RefCell::new(t))`.
pub fn rccell<T>(t: T) -> RcCell<T> {
	Rc::new(RefCell::new(t))
}

/// Radixes for displaying integer values to the user.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub enum Radix {
	Bin = 2,
	Dec = 10,
	Hex = 16,
}

// ------------------------------------------------------------------------------------------------
// DataItem
// ------------------------------------------------------------------------------------------------

/// Newtype to uniquely identify a single data item.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct DataId(pub Index);

/// A single data item.
#[derive(Debug)]
pub struct DataItem {
	id:    DataId,
	name:  Option<String>,
	ea:    EA,
	ty:    Type,
	size:  usize,
	radix: Radix,
}

impl DataItem {
	fn new(id: DataId, name: Option<String>, ea: EA, ty: Type, size: usize) -> Self {
		Self { id, name, ea, ty, size, radix: Radix::Hex }
	}

	/// Its unique ID.
	pub fn id(&self) -> DataId { self.id }
	/// Its name.
	pub fn name(&self) -> Option<&String> { self.name.as_ref() }
	/// Its EA.
	pub fn ea(&self) -> EA { self.ea }
	/// Its type.
	pub fn ty(&self) -> &Type { &self.ty }
	/// Its size in bytes.
	pub fn size(&self) -> usize { self.size }
	/// Its display radix (if that means anything).
	pub fn radix(&self) -> Radix { self.radix }

	/// Sets its display radix.
	pub fn set_radix(&mut self, new_radix: Radix) { self.radix = new_radix }
}

// ------------------------------------------------------------------------------------------------
// DataIndex
// ------------------------------------------------------------------------------------------------

/// An index of all data items in the program.
#[derive(Default)]
pub struct DataIndex {
	arena: Arena<DataItem>,
}

impl DataIndex {
	pub fn new() -> Self {
		Self { arena: Arena::new() }
	}

	/// Creates a new data item and returns its ID.
	pub fn new_item(&mut self, name: Option<String>, ea: EA, ty: Type, size: usize)
	-> DataId {
		assert!(size >= ty.min_size());

		DataId(self.arena.insert_with(move |id| {
			DataItem::new(DataId(id), name, ea, ty, size)
		}))
	}

	/// Gets the data item with the given ID.
	pub fn get(&self, id: DataId) -> &DataItem {
		self.arena.get(id.0).expect("stale DataId")
	}

	/// Same as above but mutable.
	pub fn get_mut(&mut self, id: DataId) -> &mut DataItem {
		self.arena.get_mut(id.0).expect("stale DataId")
	}

	/// Iterator over all data items in the index, in arbitrary order.
	pub fn all_items(&self) -> impl Iterator<Item = (Index, &DataItem)> {
		self.arena.iter()
	}
}

// ------------------------------------------------------------------------------------------------
// TypeSize
// ------------------------------------------------------------------------------------------------

/// The size of a type. The values in each variant are measured in bytes.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TypeSize {
	/// A fixed-size type.
	Fixed(usize),

	/// A variable-size type (i.e. a struct with a VLA). This is the minimum size of such a type.
	Variable(usize),
}

impl TypeSize {
	/// The minimum number of bytes taken up by a value of this type.
	pub fn min_size(&self) -> usize {
		match self {
			TypeSize::Fixed(s) | TypeSize::Variable(s) => *s
		}
	}

	/// Like `.unwrap()`, but for fixed type sizes.
	pub fn fixed(&self) -> usize {
		match self {
			TypeSize::Fixed(s) => *s,
			TypeSize::Variable(..) => panic!("expected a fixed size"),
		}
	}

	/// Is this a fixed-size type?
	pub fn is_fixed(&self) -> bool {
		matches!(self, TypeSize::Fixed(..))
	}
}

// ------------------------------------------------------------------------------------------------
// Type
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArrayType {
	ty:  Box<Type>,
	len: usize,
}

#[allow(clippy::len_without_is_empty)]
impl ArrayType {
	pub fn ty(&self)  -> &Type { self.ty.as_ref() }
	pub fn len(&self) -> usize { self.len }
}

impl Display for ArrayType {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}[{}]", self.ty, self.len)
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PtrType {
	to:   Box<Type>,
	kind: Box<Type>,
}

impl PtrType {
	pub fn to(&self)   -> &Type { self.to.as_ref() }
	pub fn kind(&self) -> &Type { self.kind.as_ref() }
}

impl Display for PtrType {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{} ptr to {}", self.kind, self.to)
	}
}

/// The possible types.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
	/// 1 byte, true/false.
	Bool,

	/// Signed integers.
	I8, I16, I32, I64,

	/// Unsigned integers.
	U8, U16, U32, U64,

	/// 1-byte character (probably ASCII but not necessarily!).
	Char,

	/// 2-byte character. This is likely some pre-Unicode encoding.
	WChar,

	/// A zero-terminated string of `Char` values of the given length.
	StrZ(usize),

	/// A zero-terminated string of `WChar` values of the given length.
	WStrZ(usize),

	/// An enumerated constant.
	Enum(RcCell<EnumDesc>),

	/// A bitfield.
	Bitfield(RcCell<BitfieldDesc>),

	/// A structure.
	Struct(RcCell<StructDesc>),

	/// An array of some type, with the given length.
	Array(ArrayType),

	/// A pointer to a value of type `to`. The pointer *itself* is of type `kind`.
	Ptr(PtrType),

	/// Used as the `to` type of a pointer type which points to code.
	Code,
}

impl Type {
	/// ctor for array types. Panics if `ty` is not a fixed-size type.
	pub fn array(ty: Type, len: usize) -> Self {
		assert!(ty.size().is_fixed(), "arrays can only hold fixed-size values");
		Self::Array(ArrayType { ty: Box::new(ty), len })
	}

	/// ctor for pointer types. Panics if `kind` is not a strict integer.
	pub fn ptr(to: Type, kind: Type) -> Self {
		assert!(kind.is_strict_integer(), "pointers can only be integers");
		Self::Ptr(PtrType { to: Box::new(to), kind: Box::new(kind) })
	}

	// ---------------------------------------------------------------------------------------------
	// Getters

	/// Gets the size of a single value of this type.
	pub fn size(&self) -> TypeSize {
		use Type::*;
		use TypeSize::*;

		match self {
			I8  | U8  | Bool | Char | Code => Fixed(1),
			I16 | U16 | WChar              => Fixed(2),
			I32 | U32                      => Fixed(4),
			I64 | U64                      => Fixed(8),

			StrZ(len)                      => Fixed(len + 1),
			WStrZ(len)                     => Fixed((len + 1) * 2),
			Enum(desc)                     => desc.borrow().size(),
			Bitfield(desc)                 => desc.borrow().size(),
			Struct(desc)                   => desc.borrow().size(),
			Array(ArrayType { ty, len })   => Fixed(ty.size().fixed() * len),
			Ptr(PtrType { kind, .. })      => kind.size(),
		}
	}

	/// Same as `self.size().min_size()`.
	pub fn min_size(&self) -> usize {
		self.size().min_size()
	}

	/// Same as `self.size().is_fixed()`.
	pub fn is_fixed_size(&self) -> bool {
		self.size().is_fixed()
	}

	/// Primitive types include `Bool`, integers, and `Char/WChar`.
	pub fn is_primitive(&self) -> bool {
		use Type::*;
		matches!(self, Bool | I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | Char | WChar)
	}

	/// "Strict" integers are just the primitive integer types (`U8-U64, I8-I64`).
	pub fn is_strict_integer(&self) -> bool {
		use Type::*;
		matches!(self, I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64)
	}

	/// "Loose" integers are anything represented as integers (including `Bool`, `Char/WChar`,
	/// enums, and bitfields).
	pub fn is_loose_integer(&self) -> bool {
		use Type::*;
		matches!(self, Bool | I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | Char | WChar |
			Enum(..) | Bitfield(..))
	}

	/// Only the unsigned strict integer types (`U8-U64`).
	pub fn is_strict_unsigned_integer(&self) -> bool {
		use Type::*;
		matches!(self, U8 | U16 | U32 | U64)
	}
}

impl Display for Type {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use Type::*;

		match self {
			Bool           => write!(f, "bool"),
			I8             => write!(f, "i8"),
			I16            => write!(f, "i16"),
			I32            => write!(f, "i32"),
			I64            => write!(f, "i64"),
			U8             => write!(f, "u8"),
			U16            => write!(f, "u16"),
			U32            => write!(f, "u32"),
			U64            => write!(f, "u64"),
			Char           => write!(f, "char"),
			WChar          => write!(f, "wchar"),
			StrZ(len)      => write!(f, "strz({})", len),
			WStrZ(len)     => write!(f, "wstrz({})", len),
			Enum(desc)     => write!(f, "enum {}",     desc.borrow().name()),
			Bitfield(desc) => write!(f, "bitfield {}", desc.borrow().name()),
			Struct(desc)   => write!(f, "struct {}",   desc.borrow().name()),
			Array(at)      => write!(f, "{}", at),
			Ptr(pt)        => write!(f, "{}", pt),
			Code           => write!(f, "code"),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Enums
// ------------------------------------------------------------------------------------------------

/// A single value within an enum.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumValue {
	name:  String,
	value: u64, // interpretation is up to EnumDesc::ty
	radix: Radix,
}

impl EnumValue {
	// ---------------------------------------------------------------------------------------------
	// Getters

	/// Its name.
	pub fn name(&self) -> &str   { &self.name }
	/// Its (integer) value, cast to a `u64`.
	pub fn value(&self) -> u64   { self.value }
	/// Its display radix.
	pub fn radix(&self) -> Radix { self.radix }
}

/// A descriptor of an enumerated constant type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumDesc {
	name:   String,
	ty:     Box<Type>, // the base type of the values
	values: Vec<EnumValue>,
}

impl EnumDesc {
	/// ctor. Panics if the type is not strictly integral or character, or if the name is empty.
	pub fn new(name: String, ty: Box<Type>) -> Self {
		assert!(!name.is_empty());
		EnumDesc::_check_type(ty.as_ref());
		Self { name, ty, values: Vec::new() }
	}

	// ---------------------------------------------------------------------------------------------
	// Getters

	pub fn name  (&self) -> &str            { &self.name }
	pub fn size  (&self) -> TypeSize        { self.ty.size() }
	pub fn values(&self) -> &Vec<EnumValue> { &self.values }

	pub fn has_name (&self, name: &str) -> bool { self.value_by_name(name).is_some() }
	pub fn has_value(&self, value: u64) -> bool { self.value_by_value(value).is_some() }

	/// Get the value with this name, if one exists.
	pub fn value_by_name(&self, name: &str) -> Option<&EnumValue> {
		self.values.iter().find(|&val| val.name == name)
	}

	/// Get the value with this integer value, if one exists.
	pub fn value_by_value(&self, value: u64) -> Option<&EnumValue> {
		self.values.iter().find(|&val| val.value == value)
	}

	// ---------------------------------------------------------------------------------------------
	// Mutators

	/// Change the name of this enum. Panics if name is empty.
	pub fn set_name(&mut self, new_name: &str) {
		assert!(!new_name.is_empty());
		self.name = new_name.into();
	}

	/// Change the type of this enum. The type must be strictly integral or character.
	pub fn set_type(&mut self, new_type: Box<Type>) {
		EnumDesc::_check_type(new_type.as_ref());
		self.ty = new_type;
	}

	/// Add a new value to the end of this enum. Panics if there's already a value with this name.
	pub fn add_value(&mut self, new_name: &str, new_value: u64) {
		assert!(!self.has_name(new_name));

		// use radix of previous value, or default to decimal
		let radix = self.values.last().map(|v| v.radix).unwrap_or(Radix::Dec);
		self.values.push(EnumValue { name: new_name.into(), value: new_value, radix });
	}

	/// Remove a value from this enum. Panics if there's no value with this name.
	pub fn remove_value_by_name(&mut self, name: &str) {
		match self.values.iter().position(|val| val.name == name) {
			Some(idx) => { self.values.remove(idx); }
			None      => panic!("no value named '{}' in enum '{}'", name, self.name),
		}
	}

	/// Sorts values first by value, then by name.
	pub fn sort_values(&mut self) {
		self.values.sort_unstable_by(|a, b| {
			a.value.cmp(&b.value)
			.then_with(|| a.name.cmp(&b.name))
		});
	}

	// ---------------------------------------------------------------------------------------------
	// Private

	pub fn _check_type(ty: &Type) {
		use Type::*;
		match ty {
			I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | Char | WChar => {},
			_ => panic!("invalid enum type {:?}", ty)
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Bitfields
// ------------------------------------------------------------------------------------------------

/// The allowable sizes of bitfields, in bits.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub enum BitfieldSize {
	_8  = 8,
	_16 = 16,
	_32 = 32,
}

impl BitfieldSize {
	/// The *byte* size of this *bit* size.
	pub fn size(&self) -> TypeSize {
		TypeSize::Fixed(*self as usize / 8)
	}
}

/// One field within a bitfield.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BitfieldField {
	name:      String,
	ty:        Box<Type>, // to allow e.g. sub-enums!
	radix:     Radix,
	bit_pos:   usize,     // measured from LSB=0
	bit_size:  usize,     // measured in bits
}

impl BitfieldField {
	/// ctor. `bit_pos` is its position within the bitfield and is measured from LSB=0.
	/// Panics if `ty` is not a "loose" integer type, or if `ty` is itself a bitfield type, or
	/// if the name is empty.
	pub fn new(name: &str, ty: Box<Type>, radix: Radix, bit_pos: usize, bit_size: usize) -> Self {
		assert!(!name.is_empty());
		BitfieldField::_check_type(ty.as_ref());
		Self { name: name.into(), ty, radix, bit_pos, bit_size, }
	}

	/// The bit position of the next (further-left) field after this one.
	pub fn next_bit_pos(&self) -> usize {
		self.bit_pos + self.bit_size
	}

	pub fn _check_type(ty: &Type) {
		use Type::*;
		assert!(ty.is_loose_integer() && !matches!(ty, Enum(..)), "invalid bitfield field type");
	}
}

/// A descriptor of a bitfield type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BitfieldDesc {
	name:     String,
	bit_size: BitfieldSize,
	fields:   Vec<BitfieldField>,
}

impl BitfieldDesc {
	/// ctor. Panics if the name is empty.
	pub fn new(name: &str, bit_size: BitfieldSize) -> Self {
		assert!(!name.is_empty());
		Self {
			name: name.into(),
			bit_size,
			fields: Vec::new()
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Getters

	pub fn name(&self)   -> &str                { &self.name }
	pub fn size(&self)   -> TypeSize            { self.bit_size.size() }

	/// Iterator over the fields. They are kept in position order (starting at LSB).
	pub fn fields(&self) -> &Vec<BitfieldField> { &self.fields }

	/// Is there a field with this name?
	pub fn has_field_named(&self, name: &str) -> bool { self.field_named(name).is_some() }

	/// Get the field with this name, if one exists.
	pub fn field_named(&self, name: &str) -> Option<&BitfieldField> {
		self.fields.iter().find(|&field| field.name == name)
	}

	// ---------------------------------------------------------------------------------------------
	// Mutators

	/// Change the name of this bitfield. Panics if name is empty.
	pub fn set_name(&mut self, new_name: &str) {
		assert!(!new_name.is_empty());
		self.name = new_name.into();
	}

	/// Changes the size of this bitfield. Panics if any existing fields would be
	/// truncated/deleted by the new size.
	pub fn set_size(&mut self, new_size: BitfieldSize) {
		if new_size < self.bit_size && !self.fields.is_empty() {
			assert!(self.fields.last().unwrap().next_bit_pos() <= new_size as usize);
		}

		self.bit_size = new_size;
	}

	/// Adds a new field. Panics if it overlaps any existing field.
	pub fn add_field(&mut self, field: BitfieldField) {
		let idx = match self.fields.binary_search_by(|a| a.bit_pos.cmp(&field.bit_pos)) {
			Ok(..)   => panic!("attempting to add a field at the same position as another"),
			Err(idx) => idx,
		};

		// TODO: make this more like StructDesc::add_field

		self.fields.insert(idx, field);
		self._check_invariants();
	}

	/// Removes an existing field. Panics if no field of that name exists.
	pub fn remove_field_by_name(&mut self, name: &str) {
		match self.fields.iter().position(|field| field.name == name) {
			Some(idx) => { self.fields.remove(idx); }
			None      => panic!("no field named '{}' in bitfield '{}'", name, self.name),
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Private

	pub fn _check_invariants(&self) {
		if self.fields.len() >= 2 {
			for (i, field) in self.fields[.. self.fields.len() - 1].iter().enumerate() {
				// yeah the second assert covers the first, but the first one failing
				// is more diagnostic of that particular bug
				assert!(self.fields[i + 1].bit_pos > field.bit_pos);
				assert_eq!(field.next_bit_pos(), self.fields[i + 1].bit_pos);
			}
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Structs
// ------------------------------------------------------------------------------------------------

/// A single field within a struct.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructField {
	name:   String,
	ty:     Box<Type>,
	radix:  Radix,
	offset: usize,
}

impl StructField {
	/// ctor. Use this if you don't care about the `radix`. Panics if name is empty.
	pub fn new(name: &str, ty: Box<Type>, offset: usize) -> Self {
		Self::new_radix(name, ty, Radix::Dec, offset)
	}

	/// ctor. Panics if name is empty.
	pub fn new_radix(name: &str, ty: Box<Type>, radix: Radix, offset: usize) -> Self {
		assert!(!name.is_empty());
		Self { name: name.into(), ty, radix, offset }
	}

	/// The size of this field, without padding.
	pub fn size(&self) -> TypeSize { self.ty.size() }

	/// Its offset within the structure.
	pub fn offset(&self) -> usize { self.offset }

	/// The offset of the first byte after this field. Panics if this is a VLA.
	pub fn next_offset(&self) -> usize { self.offset + self.size().fixed() }

	/// The range of offsets this field covers. Panics if this is a VLA.
	pub fn offset_range(&self) -> core::ops::Range<usize> {
		self.offset .. self.next_offset()
	}

	/// Set its offset within the structure.
	pub fn set_offset(&mut self, new_offs: usize) { self.offset = new_offs; }
}

// This weird-ass type exists to... I guess replace a pair of Options? idk
#[derive(Debug, PartialEq, Eq, Clone)]
enum VlaField {
	None,
	Just(StructField),
	WithLen(StructField, usize),
}

impl VlaField {
	pub fn is_none(&self) -> bool { self.get().is_none() }
	pub fn is_some(&self) -> bool { self.get().is_some() }
	pub fn has_len(&self) -> bool { self.get_len().is_some() }

	pub fn get_len(&self) -> Option<&usize> {
		use VlaField::*;
		match self {
			WithLen(_, len) => Some(len),
			Just(..) | None => Option::None,
		}
	}

	pub fn get_len_mut(&mut self) -> Option<&mut usize> {
		use VlaField::*;
		match self {
			WithLen(_, len) => Some(len),
			Just(..) | None => Option::None,
		}
	}

	pub fn get(&self) -> Option<&StructField> {
		use VlaField::*;
		match self {
			Just(s) | WithLen(s, ..) => Some(s),
			None => Option::None,
		}
	}

	pub fn get_mut(&mut self) -> Option<&mut StructField> {
		use VlaField::*;
		match self {
			Just(s) | WithLen(s, ..) => Some(s),
			None => Option::None,
		}
	}

	pub fn unwrap(&self) -> &StructField {
		self.get().expect("unwrapping VlaField::None")
	}

	pub fn unwrap_mut(&mut self) -> &mut StructField {
		self.get_mut().expect("unwrapping VlaField::None")
	}
}

/// A descriptor of a struct type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructDesc {
	name:    String,
	fields:  Vec<StructField>,
	size:    usize,
	vla:     VlaField,
}

impl StructDesc {
	/// ctor. Panics if name is empty.
	pub fn new(name: String) -> Self {
		Self::new_sized(name, 0)
	}

	/// ctor. Panics if name is empty.
	pub fn new_sized(name: String, size: usize) -> Self {
		assert!(!name.is_empty());
		Self { name, fields: Vec::new(), size, vla: VlaField::None }
	}

	// ---------------------------------------------------------------------------------------------
	// Getters

	/// Its name.
	pub fn name(&self) -> &str { &self.name }

	/// The size of this struct, which may or may not be fixed depending on whether
	/// it has a VLA member. The size includes any padding/empty bytes.
	pub fn size(&self) -> TypeSize {
		if let Some(vla) = self.vla.get() {
			TypeSize::Variable(self.size + vla.ty.min_size())
		} else {
			TypeSize::Fixed(self.size)
		}
	}

	/// Is this struct fixed-size?
	pub fn is_fixed_size(&self) -> bool { !self.has_vla() }

	/// Iterator over all fields.
	pub fn all_fields(&self) -> impl Iterator<Item = &StructField> {
		self.fields.iter().chain(self.vla.get())
	}

	/// Does this struct have a variable-length array member?
	pub fn has_vla(&self) -> bool { self.vla.is_some() }

	/// Gets the VLA field, if it has one.
	pub fn get_vla(&self) -> Option<&StructField> { self.vla.get() }

	/// Gets the field which represents the VLA's length, if it has one.
	pub fn get_vla_len_field(&self) -> Option<&StructField> {
		self.vla.get_len().map(|len_idx| &self.fields[*len_idx])
	}

	/// Is there a field with this name?
	pub fn has_field_named(&self, name: &str) -> bool { self.field_named(name).is_some() }

	/// Get the field with this name, if one exists.
	pub fn field_named(&self, name: &str) -> Option<&StructField> {
		self.all_fields().find(|&field| field.name == name)
	}

	/// How many normal (non-vla) fields are there?
	pub fn num_normal_fields(&self) -> usize {
		self.fields.len()
	}

	/// Get the field at this index. Gets the VLA if `idx` is the number of normal fields + 1.
	/// Panics if the index is invalid.
	pub fn nth_field(&self, idx: usize) -> &StructField {
		if idx == self.fields.len() {
			self.vla.unwrap()
		} else {
			assert!(idx < self.fields.len());
			&self.fields[idx]
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Mutators

	/// Change the name of this struct. Panics if name is empty.
	pub fn set_name(&mut self, new_name: &str) {
		assert!(!new_name.is_empty());
		self.name = new_name.into();
	}

	/// Change the size of this struct. Panics if it would delete any fields (incl. a VLA field).
	pub fn set_size(&mut self, new_size: usize) {
		if new_size < self.size {
			assert!(self.vla.is_none());

			if !self.fields.is_empty() {
				assert!(self.fields.last().unwrap().next_offset() <= new_size);
			}
		}

		self.size = new_size;
	}

	/// Add a new field. Panics if there is already a field of the same name; or if the field
	/// is not fixed-size; or if it would overlap an existing field.
	pub fn add_field(&mut self, new_field: StructField) {
		// TODO: detect & reject recursive data types

		assert!(new_field.size().is_fixed(),
			"trying to add variable-size field '{}' to struct '{}'", new_field.name, self.name);
		assert!(!self.has_field_named(&new_field.name),
			"duplicate named field '{}' in struct '{}'", new_field.name, self.name);

		// find the index of the first field that comes *after* the new one
		let next_idx = match self.fields.iter().position(|f| f.offset >= new_field.offset) {
			Some(next_idx) => next_idx,
			None           => self.fields.len(),
		};

		if next_idx < self.fields.len() {
			let next = &self.fields[next_idx];
			assert!(new_field.next_offset() <= next.offset, "new field '{}' overlaps {}::{}",
				new_field.name, self.name, next.name);
		}

		if next_idx > 0 {
			let prev = &self.fields[next_idx - 1];
			assert!(prev.next_offset() <= new_field.offset, "new field '{}' overlaps {}::{}",
				new_field.name, self.name, prev.name);
		}

		if let Some(len_idx) = self.vla.get_len_mut() {
			if *len_idx >= next_idx {
				*len_idx += 1;
			}
		}

		self.size = std::cmp::max(self.size, new_field.next_offset());
		self.fields.insert(next_idx, new_field);
	}

	/// Adds a VLA field. The field's type must be *the type of the items in the VLA.* If given
	/// an array type, that means the VLA is an *array of arrays.* The `len_field` is optional,
	/// and if given, is the index of the field which indicates the length of the VLA. Not all VLAs
	/// have an associated length field.
	///
	/// Panics if there is already a field of the same name; or if the field is not fixed-size;
	/// or if there is already a VLA field; or if `len_field` is given but is an invalid index,
	/// or refers to a field which is not an unsigned integer type.
	pub fn add_vla_field(&mut self, new_field: StructField, len_field: Option<usize>) {
		assert!(!self.has_field_named(&new_field.name), "duplicate named field '{}' in struct '{}'",
			new_field.name, self.name);
		assert!(new_field.size().is_fixed(), "cannot have variable-size VLA items in struct '{}'",
			self.name);
		assert!(self.vla.is_none(), "struct '{}' already has a VLA field", self.name);

		if let Some(len_idx) = len_field {
			assert!(len_idx < self.fields.len(), "struct '{}' VLA length field index {} is invalid",
				self.name, len_idx);

			let len_field = &self.fields[len_idx];

			assert!(len_field.ty.is_strict_unsigned_integer(),
				"struct '{}' VLA length field is a {:?}, not an unsigned int",
				self.name, len_field.ty);

			self.vla = VlaField::WithLen(new_field, len_idx);
		} else {
			self.vla = VlaField::Just(new_field);
		}
	}

	/// Remove a field by name. Will remove the VLA field if that's the one with that name.
	/// Panics if there is no field of that name, or if that field is the VLA's length field.
	pub fn remove_field_by_name(&mut self, name: &str) {
		if let Some(vla) = self.vla.get() {
			if vla.name == name {
				self.vla = VlaField::None;
				return;
			}
		}

		// only want to iterate over the non-VLA fields
		match self.fields.iter().position(|field| field.name == name) {
			Some(idx) => {
				if let Some(len_idx) = self.vla.get_len_mut() {
					assert!(*len_idx != idx,
						"trying to remove VLA index from struct '{}'", self.name);

					if *len_idx > idx {
						*len_idx -= 1;
					}
				}

				self.fields.remove(idx);
			}
			None => panic!("no field named '{}' in struct '{}'", name, self.name),
		}
	}

	/// Remove a field by index. Will remove the VLA field if given the number of non-VLA
	/// fields + 1. Panics if the given index is greater than the number of fields, or if that
	/// field is the VLA's length field.
	pub fn remove_field_by_index(&mut self, idx: usize) {
		assert!(idx <= self.fields.len());

		if idx == self.fields.len() {
			assert!(self.vla.is_some());
			self.vla = VlaField::None;
		} else {
			if let Some(len_idx) = self.vla.get_len_mut() {
				assert!(*len_idx != idx, "trying to remove VLA index from struct '{}'", self.name);

				if *len_idx > idx {
					*len_idx -= 1;
				}
			}

			self.fields.remove(idx);
		}
	}
}