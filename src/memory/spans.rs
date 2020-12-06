use std::collections::BTreeMap;
use std::fmt::Debug;

use parse_display::Display;
use derive_new::new;

use crate::analysis::BBId;

// ------------------------------------------------------------------------------------------------
// Span
// ------------------------------------------------------------------------------------------------

/// Describes a "slice" of a Segment. The start and end positions are given as offsets into the
/// segment, to avoid confusion when dealing with virtual and physical addresses.
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
#[display("{kind:?} [0x{start:08X} .. 0x{end:08X})")]
pub struct Span {
	/// address of first byte of span.
	pub start: usize,
	/// address of first byte after span.
	pub end:   usize,
	/// what kind of span it is.
	pub kind:  SpanKind,
}

impl Span {
	fn new((start, span): (&usize, &SpanInternal)) -> Self {
		Self { start: *start, end: span.end, kind: span.kind }
	}
}

/// What kind of thing the span covers.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SpanKind {
	/// Unknown (not yet analyzed)
	Unk,
	/// Code (that is, a basic block of a function)
	Code(BBId),
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
///     - adjacent unk spans are coalesced, as they have no owner.
/// 2. span map is not directly modified.
///     - exists in service of code and data indexes.
/// 3. spans can be deleted or shortened...
///     - but can't have their *starts* changed.
///     - have to delete existing span and make a new one for that.
/// 4. defined spans cannot be bisected.
///     - that leaves two non-contiguous spans with the same owner, which makes no sense
///     - but it's fine to bisect an unknown span for the same reason it's fine to coalesce them.
pub struct SpanMap {
	spans: BTreeMap<usize, SpanInternal>,
	end:   usize,
}

// The span map actually uses this type - only the end and kind fields, since the start
// is the key.
#[derive(Debug)]
#[derive(new)]
struct SpanInternal {
	end:  usize,
	kind: SpanKind,
}

impl SpanMap {
	/// Creates a new `SpanMap` with a single unknown span that covers the entire segment.
	pub fn new(size: usize) -> Self {
		let end = size;
		let mut spans = BTreeMap::new();
		spans.insert(0, SpanInternal::new(end, SpanKind::Unk));
		Self { spans, end }
	}

	/// Given an offset into the segment, gets the span which contains it.
	///
	/// # Panics
	///
	/// - if `offs` is after the last address.
	pub fn span_at(&self, offs: usize) -> Span {
		assert!(offs < self.end);
		Span::new(self.spans.range(..= offs).next_back().expect("how even"))
	}

	/// Given an offset into the segment, gets the span which comes after the containing span,
	/// or None if the containing span is the last one in the segment.
	///
	/// # Panics
	///
	/// - if `offs` is after the last address.
	pub fn span_after(&self, offs: usize) -> Option<Span> {
		assert!(offs < self.end);

		use std::ops::Bound;
		self.spans.range((Bound::Excluded(offs), Bound::Unbounded)).next().map(|a| Span::new(a))
	}

	/// Given an offset into the segment, gets the span which comes before the containing span,
	/// or None if the containing span is the first one in the segment.
	///
	/// # Panics
	///
	/// - if `offs` is after the last address.
	pub fn span_before(&self, offs: usize) -> Option<Span> {
		assert!(offs < self.end);

		let mut iter = self.spans.range(..= offs);
		iter.next_back();
		iter.next_back().map(|tup| Span::new(tup))
	}

	/// Iterator over all spans in the segment, in order.
	pub fn iter(&self) -> impl Iterator<Item = Span> + '_ {
		self.spans.iter().map(|tup| Span::new(tup))
	}

	/// Define a code or data span at `start` that stretches `len` bytes.
	///
	/// # Panics
	///
	/// - if `len` is 0.
	/// - if `kind` is `SpanKind::Unk`.
	/// - if `start` is past the end of the segment.
	/// - if `start` is not at the beginning of, or within, an unknown span.
	/// - if `start + len` is past the end of that same span.
	pub fn define(&mut self, start: usize, len: usize, kind: SpanKind) {
		assert_ne!(len, 0, "length cannot be 0");
		assert_ne!(kind, SpanKind::Unk, "must give a non-unknown span kind");
		assert!(start < self.end, "start is past end of segment");

		// find out who lives here
		let old     = self.span_at(start);
		let new_end = start + len;

		assert_eq!(old.kind, SpanKind::Unk, "defining an already-defined span");
		assert!(new_end <= old.end, "new span overflows into next span");

		// first check if we need to add a new unknown span after the new span
		if new_end < old.end {
			// make new unknown span [new_end .. old.end)
			self.spans.insert(new_end, SpanInternal::new(old.end, SpanKind::Unk));
		}

		// now let's check if we're redefining the old span, or making a new one
		if start == old.start {
			// start == old.start => redefine (and optionally resize) the old span
			let old_span  = self.spans.get_mut(&old.start).unwrap();
			old_span.kind = kind;
			old_span.end  = new_end; // no-op if new_end == old.end
		} else {
			// make the new span [start .. new_end)
			self.spans.insert(start, SpanInternal::new(new_end, kind));
			// and shorten the old one to [.. start)
			self.spans.get_mut(&old.start).unwrap().end = start;
		}

		#[cfg(debug_assertions)]
		self.check_invariants();
	}

	/// Undefine the span at `start`. Has no effect if that span is already undefined.
	/// Adjacent undefined spans are coalesced.
	///
	/// # Panics
	///
	/// - if `start` is not the beginning of a span.
	pub fn undefine(&mut self, start: usize) {
		let old = self.span_at(start);
		assert_eq!(start, old.start, "no span at this location");

		use SpanKind::Unk;

		if old.kind != Unk {
			let prev = self.span_before(start);
			let next = self.span_after(start);

			match (prev, next) {
				(Some(prev @ Span { kind: Unk, .. }), Some(next @ Span { kind: Unk, .. })) => {
					// coalesce with BOTH: delete old AND next, and make prev span longer
					self.spans.remove(&old.start).expect("wat");
					self.spans.remove(&next.start).expect("wat");
					self.spans.get_mut(&prev.start).unwrap().end = next.end;
				}

				(Some(prev @ Span { kind: Unk, .. }), _) => {
					// coalesce with prev: delete old span, and make prev span longer
					self.spans.remove(&old.start).expect("wat");
					self.spans.get_mut(&prev.start).unwrap().end = old.end;
				}

				(_, Some(next @ Span { kind: Unk, .. })) => {
					// coalesce with next: delete next span, and make old span longer
					self.spans.remove(&next.start).expect("wat");
					let old = self.spans.get_mut(&old.start).unwrap();
					old.end = next.end;
					old.kind = Unk;
				}

				_ => {
					// no coalescing to do.
					self.spans.get_mut(&old.start).unwrap().kind = Unk;
				}
			}
		}

		#[cfg(debug_assertions)]
		self.check_invariants();
	}

	#[cfg(debug_assertions)]
	fn check_invariants(&self) {
		let spans: Vec<_> = self.iter().collect();

		// INVARIANT: span map is never empty
		assert!(!spans.is_empty());

		// INVARIANT: span[0].start == 0
		assert_eq!(spans.first().unwrap().start, 0);

		// INVARIANT: span[n].end == span[n + 1].start
		// INVARIANT: span[n] and span[n + 1] can't both be undefined
		for pair in spans.windows(2) {
			match pair {
				[cur, next] => {
					assert_eq!(cur.end, next.start);
					if cur.kind == SpanKind::Unk {
						assert_ne!(next.kind, SpanKind::Unk);
					}
				}
				_ => unreachable!()
			}
		}

		// INVARIANT: span[n - 1].end == self.end
		assert_eq!(spans.last().unwrap().end, self.end);
	}

	#[cfg(any(test, debug_assertions))]
	#[allow(dead_code)]
	pub fn dump_spans(&self) {
		println!("-----------------");
		for tup in self.spans.iter() {
			println!("{}", Span::new(tup));
		}
	}
}
