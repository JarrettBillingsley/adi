use super::{ SpanMap, SpanKind, Span };
use crate::memory::{ SegId, Location };

#[test]
fn span_map() {
	use SpanKind::*;

	let id = SegId(0);
	let mut m = SpanMap::new(id, 2usize.pow(16));

	// 1. it must be all undefined at the beginning.
	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x10000),  kind: Unk },
	]);

	// 2. define in the middle of and completely covering existing ones.
	m.define(0x1000, 0x1000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x1000),  kind: Unk  },
		Span { start: Location::new(id, 0x1000), end: Location::new(id, 0x2000),  kind: Data },
		Span { start: Location::new(id, 0x2000), end: Location::new(id, 0x10000), kind: Unk  }
	]);

	m.define(0x2000, 0xE000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x1000),  kind: Unk  },
		Span { start: Location::new(id, 0x1000), end: Location::new(id, 0x2000),  kind: Data },
		Span { start: Location::new(id, 0x2000), end: Location::new(id, 0x10000), kind: Data }
	]);

	// 3. undefine with no coalescing.
	m.undefine(0x2000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x1000),  kind: Unk  },
		Span { start: Location::new(id, 0x1000), end: Location::new(id, 0x2000),  kind: Data },
		Span { start: Location::new(id, 0x2000), end: Location::new(id, 0x10000), kind: Unk  }
	]);

	// 4. undefine with coalescing on both sides.
	m.undefine(0x1000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x10000), kind: Unk  },
	]);

	// 5. define at the beginning of and the end of existing ones.
	m.define(0x0000, 0x1000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x1000),  kind: Data },
		Span { start: Location::new(id, 0x1000), end: Location::new(id, 0x10000), kind: Unk  },
	]);

	m.define(0xF000, 0x1000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x1000),  kind: Data },
		Span { start: Location::new(id, 0x1000), end: Location::new(id, 0xF000),  kind: Unk  },
		Span { start: Location::new(id, 0xF000), end: Location::new(id, 0x10000), kind: Data }
	]);

	m.define(0x1000, 0x1000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x1000),  kind: Data },
		Span { start: Location::new(id, 0x1000), end: Location::new(id, 0x2000),  kind: Data },
		Span { start: Location::new(id, 0x2000), end: Location::new(id, 0xF000),  kind: Unk  },
		Span { start: Location::new(id, 0xF000), end: Location::new(id, 0x10000), kind: Data }
	]);

	m.define(0xE000, 0x1000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x1000),  kind: Data },
		Span { start: Location::new(id, 0x1000), end: Location::new(id, 0x2000),  kind: Data },
		Span { start: Location::new(id, 0x2000), end: Location::new(id, 0xE000),  kind: Unk  },
		Span { start: Location::new(id, 0xE000), end: Location::new(id, 0xF000),  kind: Data },
		Span { start: Location::new(id, 0xF000), end: Location::new(id, 0x10000), kind: Data }
	]);

	// 6. undefine on ends
	m.undefine(0x0000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x1000),  kind: Unk },
		Span { start: Location::new(id, 0x1000), end: Location::new(id, 0x2000),  kind: Data },
		Span { start: Location::new(id, 0x2000), end: Location::new(id, 0xE000),  kind: Unk  },
		Span { start: Location::new(id, 0xE000), end: Location::new(id, 0xF000),  kind: Data },
		Span { start: Location::new(id, 0xF000), end: Location::new(id, 0x10000), kind: Data }
	]);

	m.undefine(0xF000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x1000),  kind: Unk },
		Span { start: Location::new(id, 0x1000), end: Location::new(id, 0x2000),  kind: Data },
		Span { start: Location::new(id, 0x2000), end: Location::new(id, 0xE000),  kind: Unk  },
		Span { start: Location::new(id, 0xE000), end: Location::new(id, 0xF000),  kind: Data },
		Span { start: Location::new(id, 0xF000), end: Location::new(id, 0x10000), kind: Unk }
	]);

	// 7. one-sided coalescing
	m.define(0x2000, 0xC000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x1000),  kind: Unk },
		Span { start: Location::new(id, 0x1000), end: Location::new(id, 0x2000),  kind: Data },
		Span { start: Location::new(id, 0x2000), end: Location::new(id, 0xE000),  kind: Data  },
		Span { start: Location::new(id, 0xE000), end: Location::new(id, 0xF000),  kind: Data },
		Span { start: Location::new(id, 0xF000), end: Location::new(id, 0x10000), kind: Unk }
	]);

	m.undefine(0x1000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x2000),  kind: Unk },
		Span { start: Location::new(id, 0x2000), end: Location::new(id, 0xE000),  kind: Data  },
		Span { start: Location::new(id, 0xE000), end: Location::new(id, 0xF000),  kind: Data },
		Span { start: Location::new(id, 0xF000), end: Location::new(id, 0x10000), kind: Unk }
	]);

	m.undefine(0xE000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x2000),  kind: Unk },
		Span { start: Location::new(id, 0x2000), end: Location::new(id, 0xE000),  kind: Data  },
		Span { start: Location::new(id, 0xE000), end: Location::new(id, 0x10000), kind: Unk },
	]);

	// 8. and just for good measure
	m.undefine(0x2000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x10000),  kind: Unk },
	]);
}

#[test]
fn span_map_iter() {
	use SpanKind::*;

	let id = SegId(0);
	let mut m = SpanMap::new(id, 2usize.pow(16));
	m.define(0x0000, 0x1000, Data);
	m.define(0xF000, 0x1000, Data);
	m.define(0x1000, 0x1000, Data);
	m.define(0xE000, 0x1000, Data);

	let spans = &[
		Span { start: Location::new(id, 0x0000), end: Location::new(id, 0x1000),  kind: Data },
		Span { start: Location::new(id, 0x1000), end: Location::new(id, 0x2000),  kind: Data },
		Span { start: Location::new(id, 0x2000), end: Location::new(id, 0xE000),  kind: Unk  },
		Span { start: Location::new(id, 0xE000), end: Location::new(id, 0xF000),  kind: Data },
		Span { start: Location::new(id, 0xF000), end: Location::new(id, 0x10000), kind: Data }
	];

	assert_eq!(m.iter().collect::<Vec<_>>(), spans);

	assert_eq!(m.span_at(0x0000), spans[0]);
	assert_eq!(m.span_at(0x0800), spans[0]);
	assert_eq!(m.span_at(0x0FFF), spans[0]);
	assert_eq!(m.span_at(0x1000), spans[1]);
	assert_eq!(m.span_at(0x1800), spans[1]);
	assert_eq!(m.span_at(0x1FFF), spans[1]);
	assert_eq!(m.span_at(0x2000), spans[2]);
	assert_eq!(m.span_at(0xF000), spans[4]);
	assert_eq!(m.span_at(0xFFFF), spans[4]);

	assert_eq!(m.span_before(0x0000), None);
	assert_eq!(m.span_before(0x0800), None);
	assert_eq!(m.span_before(0x0FFF), None);
	assert_eq!(m.span_before(0x1000), Some(spans[0]));
	assert_eq!(m.span_before(0x1800), Some(spans[0]));
	assert_eq!(m.span_before(0x1FFF), Some(spans[0]));
	assert_eq!(m.span_before(0x2000), Some(spans[1]));
	assert_eq!(m.span_before(0xF000), Some(spans[3]));
	assert_eq!(m.span_before(0xFFFF), Some(spans[3]));

	assert_eq!(m.span_after(0x0000), Some(spans[1]));
	assert_eq!(m.span_after(0x0800), Some(spans[1]));
	assert_eq!(m.span_after(0x0FFF), Some(spans[1]));
	assert_eq!(m.span_after(0x1000), Some(spans[2]));
	assert_eq!(m.span_after(0x1800), Some(spans[2]));
	assert_eq!(m.span_after(0x1FFF), Some(spans[2]));
	assert_eq!(m.span_after(0x2000), Some(spans[3]));
	assert_eq!(m.span_after(0xF000), None);
	assert_eq!(m.span_after(0xFFFF), None);
}

// TODO: test config, map, region, segment...