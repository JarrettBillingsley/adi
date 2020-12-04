use parse_display::*;
use derive_new::*;
use std::collections::{
	BTreeMap,
	btree_map::Values as BTreeValues,
};
use std::fmt::Debug;

use super::types::*;

// ------------------------------------------------------------------------------------------------
// Span
// ------------------------------------------------------------------------------------------------

/// Describes a "slice" of a Segment. The start and end positions are given as offsets into the
/// segment, to avoid confusion when dealing with virtual and physical addresses.
#[derive(Debug, Display)]
#[derive(new)]
#[display("")]
pub struct Span {
	/// address of first byte after span.
	pub end: SegOffset,
	/// what kind of span it is.
	pub kind: SpanKind,
}

impl Span {
	fn fmt(&self, start: SegOffset) -> String {
		format!("{:?} [0x{:08X} .. 0x{:08X})", self.kind, start, self.end)
	}
}

/// What kind of thing the span covers.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SpanKind {
	/// Unknown (not yet analyzed)
	Unk,
	/// Code (that is, a basic block of a function)
	Code(crate::analysis::types::BBId),
	/// Data (anything that isn't code)
	Data, // TODO: like, an array/variable owner type
}

// ------------------------------------------------------------------------------------------------
// SpanMap
// ------------------------------------------------------------------------------------------------

/// Representation of the "map" of a segment's spans. Has a fixed size (which matches the segment's
/// size) and the entire size is covered by spans (no "empty" spots).
///
/// Looking up spans by address is efficient (logarithmic time). Looking up spans in other ways
/// requires the use of an index.
///
/// Rules:
/// 1. spans can only go between unk and non-unk - not e.g. directly from code to data.
///     - adjacent unk spans are coalesced.
/// 2. span map is not directly modified.
///     - exists in service of code and data indexes.
/// 3. spans can be deleted or shortened...
///     - but can't have their *starts* changed.
///     - have to delete existing span and make a new one for that.
/// 4. spans cannot be bisected.
///     - that leaves two non-contiguous spans with the same owner, which makes no sense
pub struct SpanMap {
	spans: BTreeMap<SegOffset, Span>,
	end:   SegOffset,
}

impl SpanMap {
	/// Creates a new `SpanMap` with a single unknown span that covers the entire segment.
	pub fn new(size: usize) -> Self {
		let end = SegOffset(size);
		let mut spans = BTreeMap::new();
		spans.insert(SegOffset(0), Span::new(end, SpanKind::Unk));
		Self { spans, end }
	}

	/// Given an offset into the segment, gets the Span which contains it.
	pub fn span_at(&self, offs: SegOffset) -> (&SegOffset, &Span) {
		assert!(offs <= self.end); // TODO: should this be inclusive or exclusive...?
		self.spans.range(..= offs).next_back().expect("how even")
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
	pub fn iter(&self) -> BTreeValues<'_, SegOffset, Span> {
		self.spans.values()
	}

	/// Redefines a range of memory as being of a certain kind.
	pub fn redefine(&mut self,
		// TODO: SegRange for a pair of offsets (more Rustic)
		start: SegOffset, end: SegOffset, kind: SpanKind) {

		assert!(start < end);
		assert!(end <= self.end);

		// get the span inside which start falls
		let (first_start, first_end) = self.range_from_offset(start);

		// SHORTCUT: if we're just rewriting a span, don't bother splitting things up.
		if start == first_start && end == first_end {
			self.change_kind(first_start, kind);
		} else {
			// tricky: the start and end locations can come in the middle of existing spans.
			// so, we have to check if we're changing the end location of the first span or
			// the start location of the last span. in addition, there can be any number of
			// spans in between, or a single span might be bisected!
			let (_, last_end) = self.range_from_offset(end);
			let mut middle = self.spans.split_off(&first_start);
			let mut rest   = middle.split_off(&last_end);

			// at this point:
			// self.spans holds all the unaffected spans before start.
			// rest holds all the unaffected spans after end.
			// any changes will happen in middle.

			assert!(!middle.is_empty());

			let mut middle_iter = middle.range(..);
			let first = middle_iter.next().unwrap().1;
			let last  = if middle.len() > 1 { middle_iter.next_back().unwrap().1 } else { first };

			if start != first_start {
				// gotta split the first span into [first_start..start)
				self.spans.insert(first_start, Span::new(start, first.kind));
			}

			self.spans.insert(start, Span::new(end, kind));

			if end != last_end {
				// now to split the last span into [end..last_end)
				self.spans.insert(end, Span::new(last.end, last.kind));
			}

			// finally, glob on the remainder
			self.spans.append(&mut rest);
		}

		#[cfg(debug_assertions)]
		self.check_invariants();
	}

	/// Given the start of a span, change its kind.
	/// Panics if the given start offset is not the start of a span.
	pub fn change_kind(&mut self, start: SegOffset, kind: SpanKind) {
		let span = self.spans.get_mut(&start).unwrap();

		if span.kind != kind {
			span.kind = kind;
		}
	}

	fn range_from_offset(&self, offs: SegOffset) -> (SegOffset, SegOffset) {
		let (start, Span { end, .. }) = self.span_at(offs);
		(*start, *end)
	}

	#[cfg(debug_assertions)]
	fn check_invariants(&self) {
		let spans = self.spans.iter().collect::<Vec<(&SegOffset, &Span)>>();

		// INVARIANT: span map is never empty
		assert!(!spans.is_empty());

		// INVARIANT: span[0].start == 0
		assert_eq!(*spans.first().unwrap().0, SegOffset(0));

		// INVARIANT: span[n].end == span[n + 1].start
		for pair in spans.windows(2) {
			match pair {
				[(_, cur_span), (next_start, _)] => assert_eq!(cur_span.end, **next_start),
				_ => unreachable!()
			}
		}

		// INVARIANT: span[n - 1].end == self.end
		assert_eq!(spans.last().unwrap().1.end, self.end);
	}

	#[cfg(any(test, debug_assertions))]
	#[allow(dead_code)]
	pub fn dump_spans(&self) {
		println!("-----------------");
		for (start, span) in self.spans.iter() {
			println!("{}", span.fmt(*start));
		}
	}
}
