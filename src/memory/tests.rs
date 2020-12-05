use super::*;

#[test]
fn span_map() {
	use SpanKind::*;

	let mut m = SpanMap::new(2usize.pow(16));

	// 1. it must be all undefined at the beginning.
	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x10000),  kind: Unk },
	]);

	// 2. define in the middle of and completely covering existing ones.
	m.define(SegOffset(0x1000), 0x1000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x1000),  kind: Unk  },
		Span { start: SegOffset(0x1000), end: SegOffset(0x2000),  kind: Data },
		Span { start: SegOffset(0x2000), end: SegOffset(0x10000), kind: Unk  }
	]);

	m.define(SegOffset(0x2000), 0xE000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x1000),  kind: Unk  },
		Span { start: SegOffset(0x1000), end: SegOffset(0x2000),  kind: Data },
		Span { start: SegOffset(0x2000), end: SegOffset(0x10000), kind: Data }
	]);

	// 3. undefine with no coalescing.
	m.undefine(SegOffset(0x2000));

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x1000),  kind: Unk  },
		Span { start: SegOffset(0x1000), end: SegOffset(0x2000),  kind: Data },
		Span { start: SegOffset(0x2000), end: SegOffset(0x10000), kind: Unk  }
	]);

	// 4. undefine with coalescing on both sides.
	m.undefine(SegOffset(0x1000));

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x10000), kind: Unk  },
	]);

	// 5. define at the beginning of and the end of existing ones.
	m.define(SegOffset(0x0000), 0x1000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x1000),  kind: Data },
		Span { start: SegOffset(0x1000), end: SegOffset(0x10000), kind: Unk  },
	]);

	m.define(SegOffset(0xF000), 0x1000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x1000),  kind: Data },
		Span { start: SegOffset(0x1000), end: SegOffset(0xF000),  kind: Unk  },
		Span { start: SegOffset(0xF000), end: SegOffset(0x10000), kind: Data }
	]);

	m.define(SegOffset(0x1000), 0x1000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x1000),  kind: Data },
		Span { start: SegOffset(0x1000), end: SegOffset(0x2000),  kind: Data },
		Span { start: SegOffset(0x2000), end: SegOffset(0xF000),  kind: Unk  },
		Span { start: SegOffset(0xF000), end: SegOffset(0x10000), kind: Data }
	]);

	m.define(SegOffset(0xE000), 0x1000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x1000),  kind: Data },
		Span { start: SegOffset(0x1000), end: SegOffset(0x2000),  kind: Data },
		Span { start: SegOffset(0x2000), end: SegOffset(0xE000),  kind: Unk  },
		Span { start: SegOffset(0xE000), end: SegOffset(0xF000),  kind: Data },
		Span { start: SegOffset(0xF000), end: SegOffset(0x10000), kind: Data }
	]);

	// 6. undefine on ends
	m.undefine(SegOffset(0x0000));

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x1000),  kind: Unk },
		Span { start: SegOffset(0x1000), end: SegOffset(0x2000),  kind: Data },
		Span { start: SegOffset(0x2000), end: SegOffset(0xE000),  kind: Unk  },
		Span { start: SegOffset(0xE000), end: SegOffset(0xF000),  kind: Data },
		Span { start: SegOffset(0xF000), end: SegOffset(0x10000), kind: Data }
	]);

	m.undefine(SegOffset(0xF000));

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x1000),  kind: Unk },
		Span { start: SegOffset(0x1000), end: SegOffset(0x2000),  kind: Data },
		Span { start: SegOffset(0x2000), end: SegOffset(0xE000),  kind: Unk  },
		Span { start: SegOffset(0xE000), end: SegOffset(0xF000),  kind: Data },
		Span { start: SegOffset(0xF000), end: SegOffset(0x10000), kind: Unk }
	]);

	// 7. one-sided coalescing
	m.define(SegOffset(0x2000), 0xC000, Data);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x1000),  kind: Unk },
		Span { start: SegOffset(0x1000), end: SegOffset(0x2000),  kind: Data },
		Span { start: SegOffset(0x2000), end: SegOffset(0xE000),  kind: Data  },
		Span { start: SegOffset(0xE000), end: SegOffset(0xF000),  kind: Data },
		Span { start: SegOffset(0xF000), end: SegOffset(0x10000), kind: Unk }
	]);

	m.undefine(SegOffset(0x1000));

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x2000),  kind: Unk },
		Span { start: SegOffset(0x2000), end: SegOffset(0xE000),  kind: Data  },
		Span { start: SegOffset(0xE000), end: SegOffset(0xF000),  kind: Data },
		Span { start: SegOffset(0xF000), end: SegOffset(0x10000), kind: Unk }
	]);

	m.undefine(SegOffset(0xE000));

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x2000),  kind: Unk },
		Span { start: SegOffset(0x2000), end: SegOffset(0xE000),  kind: Data  },
		Span { start: SegOffset(0xE000), end: SegOffset(0x10000), kind: Unk },
	]);

	// 8. and just for good measure
	m.undefine(SegOffset(0x2000));

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x10000),  kind: Unk },
	]);

}

#[test]
fn span_map_iter() {
	use SpanKind::*;

	let mut m = SpanMap::new(2usize.pow(16));
	m.define(SegOffset(0x0000), 0x1000, Data);
	m.define(SegOffset(0xF000), 0x1000, Data);
	m.define(SegOffset(0x1000), 0x1000, Data);
	m.define(SegOffset(0xE000), 0x1000, Data);

	let spans = &[
		Span { start: SegOffset(0x0000), end: SegOffset(0x1000),  kind: Data },
		Span { start: SegOffset(0x1000), end: SegOffset(0x2000),  kind: Data },
		Span { start: SegOffset(0x2000), end: SegOffset(0xE000),  kind: Unk  },
		Span { start: SegOffset(0xE000), end: SegOffset(0xF000),  kind: Data },
		Span { start: SegOffset(0xF000), end: SegOffset(0x10000), kind: Data }
	];

	assert_eq!(m.iter().collect::<Vec<_>>(), spans);

	assert_eq!(m.span_at(SegOffset(0x0000)), spans[0]);
	assert_eq!(m.span_at(SegOffset(0x0800)), spans[0]);
	assert_eq!(m.span_at(SegOffset(0x0FFF)), spans[0]);
	assert_eq!(m.span_at(SegOffset(0x1000)), spans[1]);
	assert_eq!(m.span_at(SegOffset(0x1800)), spans[1]);
	assert_eq!(m.span_at(SegOffset(0x1FFF)), spans[1]);
	assert_eq!(m.span_at(SegOffset(0x2000)), spans[2]);
	assert_eq!(m.span_at(SegOffset(0xF000)), spans[4]);
	assert_eq!(m.span_at(SegOffset(0xFFFF)), spans[4]);

	assert_eq!(m.span_before(SegOffset(0x0000)), None);
	assert_eq!(m.span_before(SegOffset(0x0800)), None);
	assert_eq!(m.span_before(SegOffset(0x0FFF)), None);
	assert_eq!(m.span_before(SegOffset(0x1000)), Some(spans[0]));
	assert_eq!(m.span_before(SegOffset(0x1800)), Some(spans[0]));
	assert_eq!(m.span_before(SegOffset(0x1FFF)), Some(spans[0]));
	assert_eq!(m.span_before(SegOffset(0x2000)), Some(spans[1]));
	assert_eq!(m.span_before(SegOffset(0xF000)), Some(spans[3]));
	assert_eq!(m.span_before(SegOffset(0xFFFF)), Some(spans[3]));

	assert_eq!(m.span_after(SegOffset(0x0000)), Some(spans[1]));
	assert_eq!(m.span_after(SegOffset(0x0800)), Some(spans[1]));
	assert_eq!(m.span_after(SegOffset(0x0FFF)), Some(spans[1]));
	assert_eq!(m.span_after(SegOffset(0x1000)), Some(spans[2]));
	assert_eq!(m.span_after(SegOffset(0x1800)), Some(spans[2]));
	assert_eq!(m.span_after(SegOffset(0x1FFF)), Some(spans[2]));
	assert_eq!(m.span_after(SegOffset(0x2000)), Some(spans[3]));
	assert_eq!(m.span_after(SegOffset(0xF000)), None);
	assert_eq!(m.span_after(SegOffset(0xFFFF)), None);
}

// TODO: test config, map, region, segment...