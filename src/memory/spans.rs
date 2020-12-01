use parse_display::*;
use derive_new::*;
use std::collections::{
	BTreeMap,
	btree_map::Values as BTreeValues,
};
use std::fmt::{ Debug, Display };

use super::types::*;

// ------------------------------------------------------------------------------------------------
// Span
// ------------------------------------------------------------------------------------------------

// TODO: what IS this?
pub trait SpanOwner: Debug + Display {}

/// Describes a "slice" of a Segment. The start and end positions are given as offsets into the
/// segment, to avoid confusion when dealing with virtual and physical addresses.
///
/// Its "owner" is some kind of object which "manages" this span. For example, code spans can be
/// managed by BasicBlock objects. (data spans might have some kind of Array or Variable object?)
#[derive(Debug, Display)]
#[derive(new)]
#[display("{kind} [0x{start:08X} .. 0x{end:08X})")]
pub struct Span<'a> {
	/// address of first byte.
	pub start: SegOffset,
	/// address of first byte after span.
	pub end: SegOffset,
	/// what kind of span it is.
	pub kind: SpanKind,
	/// the owner, if any.
	pub owner: Option<&'a dyn SpanOwner>,
}

/// What kind of thing the span covers.
#[derive(Debug, Display, PartialEq, Eq, Clone, Copy)]
pub enum SpanKind {
	/// Unknown (not yet analyzed)
	Unk,
	/// Code (that is, a basic block of a function)
	Code,
	/// Data (anything that isn't code)
	Data,
}

// ------------------------------------------------------------------------------------------------
// SpanMap
// ------------------------------------------------------------------------------------------------

/// Representation of the "map" of a segment's spans. Has a fixed size (which matches the segment's
/// size) and the entire size is covered by spans (no "empty" spots).
///
/// Looking up spans by address is efficient (logarithmic time). Looking up spans in other ways
/// requires the use of an index.
pub struct SpanMap<'a> {
	// There's some duplication between the index into the map and Span::start.
	// Ideally we'd use BTreeSet using Span::start as the sorting key.
	// But I don't think it's possible to use BTreeSet::range using a SegOffset as
	// the range bounds when the value is a Span. So, a map (and duplication) it is.
	spans: BTreeMap<SegOffset, Span<'a>>,
	end:   SegOffset,
}

impl<'a> SpanMap<'a> {
	/// Creates a new `SpanMap` with a single unknown span that covers the entire segment.
	pub fn new(size: usize) -> Self {
		let end = SegOffset(size);
		let mut spans = BTreeMap::new();
		spans.insert(SegOffset(0), Span::new(SegOffset(0), end, SpanKind::Unk, None));
		Self { spans, end }
	}

	/// Given an offset into the segment, gets the Span which contains it.
	pub fn span_at(&self, offs: SegOffset) -> &Span {
		assert!(offs <= self.end); // TODO: should this be inclusive or exclusive...?
		self.spans.range(..= offs).next_back().expect("how even").1
	}

	/// Given an offset into the segment, gets the Span which comes after the containing Span,
	/// or None if the containing Span is the last one in the segment.
	pub fn span_after(&self, offs: SegOffset) -> Option<&Span> {
		assert!(offs <= self.end); // TODO: should this be inclusive or exclusive...?

		if self.spans.contains_key(&offs) {
			self.spans.range(offs + 1 ..)
		} else {
			self.spans.range(offs ..)
		}.next().map(|(_, a)| a)
	}

	/// Iterator over all spans (SegOffset, Span).
	pub fn iter(&self) -> BTreeValues<'a, SegOffset, Span> {
		self.spans.values()
	}

	/// Redefines a range of memory as being of a certain kind with a given owner.
	pub fn redefine(&mut self,
		// TODO: SegRange for a pair of offsets (more Rustic)
		start: SegOffset, end: SegOffset, kind: SpanKind, owner: Option<&'a dyn SpanOwner>) {

		assert!(start < end);
		assert!(end <= self.end);

		// get the span inside which start falls
		let (first_start, first_end) = self.range_from_offset(start);

		// SHORTCUT: if we're just rewriting a span, don't bother splitting things up.
		if start == first_start && end == first_end {
			self.rewrite(first_start, kind, owner);
		} else {
			let (_, last_end) = self.range_from_offset(end);
			let mut middle = self.spans.split_off(&first_start);
			let mut rest   = middle.split_off(&last_end);

			// at this point:
			// self.spans holds all the unaffected spans before start.
			// rest holds all the unaffected spans after end.
			// any changes will happen in middle.

			assert!(middle.len() >= 1);

			let mut middle_iter = middle.range(..);
			let first = middle_iter.next().unwrap().1;
			let last  = if middle.len() > 1 { middle_iter.next_back().unwrap().1 } else { first };

			// TODO: update indexes
			if start != first_start {
				// gotta split the first span into [first_start..start)
				self.spans.insert(first_start, Span::new(first.start, start, first.kind, first.owner));
			}

			self.spans.insert(start, Span::new(start, end, kind, owner));

			if end != last_end {
				// now to split the last span into [end..last_end)
				self.spans.insert(end, Span::new(end, last.end, last.kind, last.owner));
			}

			// finally, glob on the remainder
			self.spans.append(&mut rest);
		}

		#[cfg(debug_assertions)]
		self.check_invariants();
	}

	fn rewrite(&mut self, start: SegOffset, kind: SpanKind, owner: Option<&'a dyn SpanOwner>) {
		println!("rewriting 0x{:08X}!", start);
		let span = self.spans.get_mut(&start).unwrap();
		span.kind = kind;
		span.owner = owner;
		// TODO: update indexes
	}

	fn range_from_offset(&self, offs: SegOffset) -> (SegOffset, SegOffset) {
		let Span { start, end, .. } = *self.span_at(offs);
		(start, end)
	}

	#[cfg(debug_assertions)]
	fn check_invariants(&self) {
		// INVARIANT: span[i].end == span[i + 1].start
		let spans = self.spans.values().collect::<Vec<_>>();

		for i in 0 .. spans.len() - 1 {
			assert_eq!(spans[i].end, spans[i + 1].start);
		}

		// INVARIANT: span[n - 1].end == self.end
		assert_eq!(spans[spans.len() - 1].end, self.end);
	}

	#[cfg(any(test, debug_assertions))]
	#[allow(dead_code)]
	pub fn dump_spans(&self) {
		println!("-----------------");
		for (_, s) in &self.spans {
			println!("{}", s);
		}
	}
}
