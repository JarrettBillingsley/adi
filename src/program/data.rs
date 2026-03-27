
use std::hash::{ Hash };
use std::collections::{ HashMap };
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use delegate::delegate;
use generational_arena::{ Arena, Index };

use crate::{ Size, Offs, to_usize };
use crate::memory::{ EA };

// ------------------------------------------------------------------------------------------------
// Misc
// ------------------------------------------------------------------------------------------------

/// Radixes for displaying integer values to the user.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub enum Radix {
	Bin = 2,
	Dec = 10,
	Hex = 16,
}

// ------------------------------------------------------------------------------------------------
// DataId
// ------------------------------------------------------------------------------------------------

/// Newtype to uniquely identify a single data item.
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct DataId(pub Index);

impl Debug for DataId {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		let (index, generation) = self.0.into_raw_parts();
		write!(f, "DataId({}, {})", index, generation)
	}
}

// ------------------------------------------------------------------------------------------------
// DataItem
// ------------------------------------------------------------------------------------------------

/// A single data item. This is to data what a [`BasicBlock`] is to code. Spans in a `Segment` can
/// correspond to either code (a `BasicBlock`) or data (a `DataItem`).
#[derive(Debug)]
pub struct DataItem {
	id:    DataId,
	ea:    EA,
	ty:    Type,
	size:  Size,
	radix: Radix,
}

impl DataItem {
	fn new(id: DataId, ea: EA, ty: &Type, size: Size) -> Self {
		Self { id, ea, ty: ty.clone(), size, radix: Radix::Hex }
	}

	/// Its unique ID.
	pub fn id(&self) -> DataId { self.id }
	/// Its EA.
	pub fn ea(&self) -> EA { self.ea }
	/// Its type.
	pub fn ty(&self) -> &Type { &self.ty }
	/// Its size in bytes.
	pub fn size(&self) -> Size { self.size }
	/// Its display radix (if that means anything).
	pub fn radix(&self) -> Radix { self.radix }

	/// Sets its display radix.
	pub fn set_radix(&mut self, new_radix: Radix) { self.radix = new_radix }
}

// ------------------------------------------------------------------------------------------------
// DataIndex
// ------------------------------------------------------------------------------------------------

/// An index of all data items in the program.
pub struct DataIndex {
	arena: Arena<DataItem>,
	types: TypeIndex,
}

impl DataIndex {
	#[allow(clippy::new_without_default)]
	pub fn new() -> Self {
		Self {
			arena: Arena::new(),
			types: TypeIndex::new(),
		}
	}

	/// Creates a new data item and returns its ID.
	pub fn new_item(&mut self, ea: EA, ty: &Type, size: Size) -> DataId {
		assert!(size >= self.types.min_sizeof(ty));

		DataId(self.arena.insert_with(move |id| {
			DataItem::new(DataId(id), ea, ty, size)
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

	delegate!{
		to self.types {
			pub fn array(&self, item_ty: &Type, len: Size) -> Type;
			pub fn ptr(&self, to: &Type, kind: &Type) -> Type;
			pub fn new_struct(&mut self, name: &str) -> StructId;
			pub fn new_struct_sized(&mut self, name: &str, size: Size) -> StructId;
			pub fn new_enum(&mut self, name: &str, ty: Box<Type>) -> EnumId;
			pub fn new_bitfield(&mut self, name: &str, bit_size: BitfieldSize) -> BitfieldId;
			pub fn sizeof(&self, ty: &Type) -> TypeSize;
			pub fn min_sizeof(&self, ty: &Type) -> Size;
			pub fn is_fixed_size(&self, ty: &Type) -> bool;
		}
	}
}

// ------------------------------------------------------------------------------------------------
// TypeSize
// ------------------------------------------------------------------------------------------------

/// The size of a type. The values in each variant are measured in bytes.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TypeSize {
	/// A fixed-size type.
	Fixed(Size),

	/// A variable-size type (i.e. a struct with a VLA). This is the minimum size of such a type.
	Variable(Size),
}

impl TypeSize {
	/// The minimum number of bytes taken up by a value of this type.
	pub fn min_size(&self) -> Size {
		match self {
			TypeSize::Fixed(s) | TypeSize::Variable(s) => *s
		}
	}

	/// Like `.unwrap()`, but for fixed type sizes.
	pub fn fixed(&self) -> Size {
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
// StructId, EnumId, BitfieldId
// ------------------------------------------------------------------------------------------------

/// Newtype which uniquely identifies a struct type.
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct StructId(Offs);

impl Debug for StructId {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "StructId({})", self.0)
	}
}

/// Newtype which uniquely identifies an enum type.
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct EnumId(Offs);

impl Debug for EnumId {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "EnumId({})", self.0)
	}
}

/// Newtype which uniquely identifies a bitfield type.
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct BitfieldId(Offs);

impl Debug for BitfieldId {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "BitfieldId({})", self.0)
	}
}

// ------------------------------------------------------------------------------------------------
// CustomTypeIndex, StructIndex, EnumIndex, BitfieldIndex
// ------------------------------------------------------------------------------------------------

pub trait IId: Copy + Eq + Hash {
	fn new(offs: Offs) -> Self;
}

impl IId for EnumId     { fn new(offs: Offs) -> Self { Self(offs) } }
impl IId for StructId   { fn new(offs: Offs) -> Self { Self(offs) } }
impl IId for BitfieldId { fn new(offs: Offs) -> Self { Self(offs) } }

pub struct CustomTypeIndex<T, Id> {
	arena: HashMap<Id, T>,
	next:  Offs,
}

impl<T, Id: IId> CustomTypeIndex<T, Id> {
	#[allow(clippy::new_without_default)]
	pub fn new() -> Self {
		Self { arena: HashMap::new(), next: 0 }
	}

	fn insert(&mut self, value: T) -> Id {
		let id = Id::new(self.next);
		self.next += 1;
		self.arena.insert(id, value);
		id
	}

	pub fn get(&self, id: Id) -> &T {
		self.arena.get(&id).expect("stale Id")
	}

	pub fn get_mut(&mut self, id: Id) -> &mut T {
		self.arena.get_mut(&id).expect("stale Id")
	}

	pub fn all_enums(&self) -> impl Iterator<Item = (Id, &T)> {
		self.arena.iter().map(|(id, desc)| (*id, desc))
	}
}

type StructIndex   = CustomTypeIndex<StructDesc,   StructId>;
type EnumIndex     = CustomTypeIndex<EnumDesc,     EnumId>;
type BitfieldIndex = CustomTypeIndex<BitfieldDesc, BitfieldId>;

// ------------------------------------------------------------------------------------------------
// TypeIndex
// ------------------------------------------------------------------------------------------------

/// An index of all custom types in the program.
pub struct TypeIndex {
	structs:   StructIndex,
	enums:     EnumIndex,
	bitfields: BitfieldIndex,
}

impl TypeIndex {
	#[allow(clippy::new_without_default)]
	pub fn new() -> Self {
		Self {
			structs:   StructIndex::new(),
			enums:     EnumIndex::new(),
			bitfields: BitfieldIndex::new(),
		}
	}

	/// Create an array type. Panics if `item_ty` is not a fixed-size type.
	pub fn array(&self, item_ty: &Type, len: Size) -> Type {
		assert!(self.is_fixed_size(&item_ty), "arrays can only hold fixed-size values");
		Type::Array(ArrayType { ty: Box::new(item_ty.clone()), len })
	}

	/// Create a pointer type. Panics if `kind` is not a strict integer.
	pub fn ptr(&self, to: &Type, kind: &Type) -> Type {
		assert!(kind.is_strict_integer(), "pointers can only be integers");
		Type::Ptr(PtrType { to: Box::new(to.clone()), kind: Box::new(kind.clone()) })
	}

	/// Create a new struct type.
	///
	/// Panics if name is empty.
	pub fn new_struct(&mut self, name: &str) -> StructId {
		self.structs.insert(StructDesc::new(self.structs.next, name))
	}

	/// Create a new struct type of a given size.
	///
	/// Panics if name is empty.
	pub fn new_struct_sized(&mut self, name: &str, size: Size) -> StructId {
		self.structs.insert(StructDesc::new_sized(self.structs.next, name, size))
	}

	/// Create a new enum type.
	///
	/// Panics if the type is not strictly integral or character, or if the name is empty.
	pub fn new_enum(&mut self, name: &str, ty: Box<Type>) -> EnumId {
		self.enums.insert(EnumDesc::new(self.enums.next, name, ty))
	}

	/// Create a new bitfield type.
	///
	/// Panics if the name is empty.
	pub fn new_bitfield(&mut self, name: &str, bit_size: BitfieldSize) -> BitfieldId {
		self.bitfields.insert(BitfieldDesc::new(self.bitfields.next, name, bit_size))
	}

	/// Gets the size of a single value of `ty`.
	pub fn sizeof(&self, ty: &Type) -> TypeSize {
		ty.size(self)
	}

	/// Same as `self.sizeof(ty).min_size()`.
	pub fn min_sizeof(&self, ty: &Type) -> Size {
		ty.size(self).min_size()
	}

	/// Same as `self.sizeof(ty).is_fixed()`.
	pub fn is_fixed_size(&self, ty: &Type) -> bool {
		ty.size(self).is_fixed()
	}
}

// ------------------------------------------------------------------------------------------------
// Type
// ------------------------------------------------------------------------------------------------

#[derive(PartialEq, Eq, Clone)]
pub struct ArrayType {
	ty:  Box<Type>,
	len: Size,
}

#[allow(clippy::len_without_is_empty)]
impl ArrayType {
	pub fn ty(&self)  -> &Type { self.ty.as_ref() }
	pub fn len(&self) -> Size { self.len }
}

impl Debug for ArrayType {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{:?}[{}]", self.ty, self.len)
	}
}

#[derive(PartialEq, Eq, Clone)]
pub struct PtrType {
	to:   Box<Type>,
	kind: Box<Type>,
}

impl PtrType {
	pub fn to(&self)   -> &Type { self.to.as_ref() }
	pub fn kind(&self) -> &Type { self.kind.as_ref() }
}

impl Debug for PtrType {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{:?} ptr to {:?}", self.kind, self.to)
	}
}

/// The possible types.
#[derive(PartialEq, Eq, Clone)]
pub enum Type {
	/// 1 byte, true/false.
	Bool,

	/// Signed integers.
	S8, S16, S32, S64,

	/// Unsigned integers.
	U8, U16, U32, U64,

	/// 1-byte character (probably ASCII but not necessarily!).
	Char,

	/// 2-byte character. This is likely some pre-Unicode encoding.
	WChar,

	/// A zero-terminated string of `Char` values of the given length.
	StrZ(Size),

	/// A zero-terminated string of `WChar` values of the given length.
	WStrZ(Size),

	/// An enumerated constant.
	Enum(EnumId),

	/// A bitfield.
	Bitfield(BitfieldId),

	/// A structure.
	Struct(StructId),

	/// An array of some type, with the given length.
	Array(ArrayType),

	/// A pointer to a value of type `to`. The pointer *itself* is of type `kind`.
	Ptr(PtrType),

	/// Used as the `to` type of a pointer type which points to code.
	Code,
}

impl Type {
	// ---------------------------------------------------------------------------------------------
	// Getters

	/// Gets the size of a single value of this type.
	pub fn size(&self, idx: &TypeIndex) -> TypeSize {
		use Type::*;
		use TypeSize::*;

		match self {
			S8  | U8  | Bool | Char | Code => Fixed(1),
			S16 | U16 | WChar              => Fixed(2),
			S32 | U32                      => Fixed(4),
			S64 | U64                      => Fixed(8),

			StrZ(len)                      => Fixed(len + 1),
			WStrZ(len)                     => Fixed((len + 1) * 2),
			Enum(id)                       => idx.enums    .get(*id).size(idx),
			Bitfield(id)                   => idx.bitfields.get(*id).size(),
			Struct(id)                     => idx.structs  .get(*id).size(idx),
			Array(ArrayType { ty, len })   => Fixed(idx.sizeof(ty).fixed() * len),
			Ptr(PtrType { kind, .. })      => idx.sizeof(kind),
		}
	}
	/// Primitive types include `Bool`, integers, and `Char/WChar`.
	pub fn is_primitive(&self) -> bool {
		use Type::*;
		matches!(self, Bool | S8 | S16 | S32 | S64 | U8 | U16 | U32 | U64 | Char | WChar)
	}

	/// "Strict" integers are just the primitive integer types (`U8-U64, S8-S64`).
	pub fn is_strict_integer(&self) -> bool {
		use Type::*;
		matches!(self, S8 | S16 | S32 | S64 | U8 | U16 | U32 | U64)
	}

	/// "Loose" integers are anything represented as integers (including `Bool`, `Char/WChar`,
	/// enums, and bitfields).
	pub fn is_loose_integer(&self) -> bool {
		use Type::*;
		matches!(self, Bool | S8 | S16 | S32 | S64 | U8 | U16 | U32 | U64 | Char | WChar |
			Enum(..) | Bitfield(..))
	}

	/// Only the unsigned strict integer types (`U8-U64`).
	pub fn is_strict_unsigned_integer(&self) -> bool {
		use Type::*;
		matches!(self, U8 | U16 | U32 | U64)
	}
}

impl Debug for Type {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use Type::*;

		match self {
			Bool         => write!(f, "bool"),
			S8           => write!(f, "S8"),
			S16          => write!(f, "S16"),
			S32          => write!(f, "S32"),
			S64          => write!(f, "S64"),
			U8           => write!(f, "u8"),
			U16          => write!(f, "u16"),
			U32          => write!(f, "u32"),
			U64          => write!(f, "u64"),
			Char         => write!(f, "char"),
			WChar        => write!(f, "wchar"),
			StrZ(len)    => write!(f, "strz({})", len),
			WStrZ(len)   => write!(f, "wstrz({})", len),
			Enum(id)     => write!(f, "enum(id = {})",     id.0),
			Bitfield(id) => write!(f, "bitfield(id = {})", id.0),
			Struct(id)   => write!(f, "struct(id = {})",   id.0),
			Array(at)    => write!(f, "{:?}", at),
			Ptr(pt)      => write!(f, "{:?}", pt),
			Code         => write!(f, "code"),
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
	segoffs: Offs, // offset into the enums segment
	name:    String,
	ty:      Box<Type>, // the base type of the values
	values:  Vec<EnumValue>,
}

impl EnumDesc {
	fn new(segoffs: Offs, name: &str, ty: Box<Type>) -> Self {
		assert!(!name.is_empty());
		Self::check_type(ty.as_ref());
		Self {
			segoffs,
			name: name.into(),
			ty,
			values: Vec::new()
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Getters

	pub fn id    (&self) -> EnumId          { EnumId(self.segoffs) }
	pub fn ea    (&self) -> EA              { EA::new_enums(self.segoffs) }
	pub fn name  (&self) -> &str            { &self.name }
	pub fn values(&self) -> &Vec<EnumValue> { &self.values }

	pub fn size(&self, idx: &TypeIndex) -> TypeSize {
		self.ty.size(idx)
	}

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
		EnumDesc::check_type(new_type.as_ref());
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

	pub fn check_type(ty: &Type) {
		use Type::*;
		match ty {
			S8 | S16 | S32 | S64 | U8 | U16 | U32 | U64 | Char | WChar => {},
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
		TypeSize::Fixed(*self as Size / 8)
	}
}

/// One field within a bitfield.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BitfieldField {
	name:      String,
	ty:        Box<Type>, // to allow e.g. sub-enums!
	radix:     Radix,
	bit_pos:   Offs,     // measured from LSB=0
	bit_size:  Offs,     // measured in bits
}

impl BitfieldField {
	/// ctor. `bit_pos` is its position within the bitfield and is measured from LSB=0.
	/// Panics if `ty` is not a "loose" integer type, or if `ty` is itself a bitfield type, or
	/// if the name is empty.
	pub fn new(name: &str, ty: Box<Type>, radix: Radix, bit_pos: Offs, bit_size: Offs) -> Self {
		assert!(!name.is_empty());
		BitfieldField::check_type(ty.as_ref());
		Self { name: name.into(), ty, radix, bit_pos, bit_size, }
	}

	/// The bit position of the next (further-left) field after this one.
	pub fn next_bit_pos(&self) -> Offs {
		self.bit_pos + self.bit_size
	}

	pub fn check_type(ty: &Type) {
		use Type::*;
		assert!(ty.is_loose_integer() && !matches!(ty, Enum(..)), "invalid bitfield field type");
	}
}

/// A descriptor of a bitfield type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BitfieldDesc {
	segoffs:  Offs, // offset into the bitfields segment
	name:     String,
	bit_size: BitfieldSize,
	fields:   Vec<BitfieldField>,
}

impl BitfieldDesc {
	/// ctor. Panics if the name is empty.
	pub fn new(segoffs: Offs, name: &str, bit_size: BitfieldSize) -> Self {
		assert!(!name.is_empty());
		Self {
			segoffs,
			name: name.into(),
			bit_size,
			fields: Vec::new()
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Getters

	pub fn id  (&self) -> BitfieldId  { BitfieldId(self.segoffs) }
	pub fn ea  (&self) -> EA          { EA::new_bitfields(self.segoffs) }
	pub fn name(&self) -> &str        { &self.name }
	pub fn size(&self) -> TypeSize    { self.bit_size.size() }

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
			assert!(self.fields.last().unwrap().next_bit_pos() <= new_size as Size);
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
	offset: Offs,
}

impl StructField {
	/// ctor. Use this if you don't care about the `radix`. Panics if name is empty.
	pub fn new(name: &str, ty: Box<Type>, offset: Offs) -> Self {
		Self::new_radix(name, ty, Radix::Dec, offset)
	}

	/// ctor. Panics if name is empty.
	pub fn new_radix(name: &str, ty: Box<Type>, radix: Radix, offset: Offs) -> Self {
		assert!(!name.is_empty());
		Self { name: name.into(), ty, radix, offset }
	}

	/// The size of this field, without padding.
	pub fn size(&self, idx: &TypeIndex) -> TypeSize {
		self.ty.size(idx)
	}

	/// Its offset within the structure.
	pub fn offset(&self) -> Offs { self.offset }

	/// The offset of the first byte after this field. Panics if this is a VLA.
	pub fn next_offset(&self, idx: &TypeIndex) -> Offs {
		self.offset + self.size(idx).fixed()
	}

	/// The range of offsets this field covers. Panics if this is a VLA.
	pub fn offset_range(&self, idx: &TypeIndex) -> core::ops::Range<Offs> {
		self.offset .. self.next_offset(idx)
	}

	/// Set its offset within the structure.
	pub fn set_offset(&mut self, new_offs: Offs) { self.offset = new_offs; }
}

// This weird-ass type exists to... I guess replace a pair of Options? idk
#[derive(Debug, PartialEq, Eq, Clone)]
enum VlaField {
	None,
	Just(StructField),
	/// .1 is the index of the field which specifies the length
	WithLen(StructField, usize),
}

impl VlaField {
	pub fn is_none(&self) -> bool { matches!(self, VlaField::None) }
	pub fn is_some(&self) -> bool { matches!(self, VlaField::Just(..)) }
	pub fn has_len(&self) -> bool { matches!(self, VlaField::WithLen(..)) }

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
	segoffs: Offs, // offset into the structs segment
	name:    String,
	fields:  Vec<StructField>,
	size:    Size,
	vla:     VlaField,
}

impl StructDesc {
	fn new(segoffs: Offs, name: &str) -> Self {
		Self::new_sized(segoffs, name, 0)
	}

	fn new_sized(segoffs: Offs, name: &str, size: Size) -> Self {
		assert!(!name.is_empty());
		Self {
			segoffs,
			name:   name.into(),
			size,
			fields: Vec::new(),
			vla:    VlaField::None
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Getters

	pub fn id    (&self) -> StructId { StructId(self.segoffs) }
	pub fn ea    (&self) -> EA       { EA::new_structs(self.segoffs) }

	/// Its name.
	pub fn name(&self) -> &str { &self.name }

	fn size(&self, idx: &TypeIndex) -> TypeSize {
		if let Some(vla) = self.vla.get() {
			TypeSize::Variable(self.size + idx.min_sizeof(&vla.ty))
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
	pub fn num_normal_fields(&self) -> Size {
		self.fields.len() as Size
	}

	/// Get the field at this index. Gets the VLA if `idx` is the number of normal fields + 1.
	/// Panics if the index is invalid.
	pub fn nth_field(&self, idx: Offs) -> &StructField {
		if idx == self.fields.len() as Size {
			self.vla.unwrap()
		} else {
			assert!(idx < self.fields.len() as Size);
			&self.fields[to_usize(idx)]
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
	pub fn set_size(&mut self, new_size: Size, idx: &TypeIndex) {
		if new_size < self.size {
			assert!(self.vla.is_none());

			if !self.fields.is_empty() {
				assert!(self.fields.last().unwrap().next_offset(idx) <= new_size);
			}
		}

		self.size = new_size;
	}

	/// Add a new field. Panics if there is already a field of the same name; or if the field
	/// is not fixed-size; or if it would overlap an existing field.
	pub fn add_field(&mut self, new_field: StructField, idx: &TypeIndex) {
		// TODO: detect & reject recursive data types

		assert!(new_field.size(idx).is_fixed(),
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
			assert!(new_field.next_offset(idx) <= next.offset, "new field '{}' overlaps {}::{}",
				new_field.name, self.name, next.name);
		}

		if next_idx > 0 {
			let prev = &self.fields[next_idx - 1];
			assert!(prev.next_offset(idx) <= new_field.offset, "new field '{}' overlaps {}::{}",
				new_field.name, self.name, prev.name);
		}

		if let Some(len_idx) = self.vla.get_len_mut() && *len_idx >= next_idx {
			*len_idx += 1;
		}

		self.size = std::cmp::max(self.size, new_field.next_offset(idx));
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
	pub fn add_vla_field(&mut self, new_field: StructField, len_field: Option<usize>,
	idx: &TypeIndex) {
		assert!(!self.has_field_named(&new_field.name), "duplicate named field '{}' in struct '{}'",
			new_field.name, self.name);
		assert!(new_field.size(idx).is_fixed(), "cannot have variable-size VLA items in struct '{}'",
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
		if let Some(vla) = self.vla.get() && vla.name == name {
			self.vla = VlaField::None;
			return;
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