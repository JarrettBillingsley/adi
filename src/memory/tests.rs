use super::{ SpanMap, SpanKind, Span };
use crate::memory::{ SegId };

#[test]
fn span_map() {
	use SpanKind::*;

	let id = SegId(0);
	let mut m = SpanMap::new(id, 2usize.pow(16));

	// 1. it must be all undefined at the beginning.
	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x10000,  Unk ),
	]);

	// 2. define in the middle of and completely covering existing ones.
	m.define(0x1000, 0x1000, Ana);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x1000,  Unk  ),
		Span::new(id, 0x1000, 0x2000,  Ana ),
		Span::new(id, 0x2000, 0x10000, Unk  ),
	]);

	m.define(0x2000, 0xE000, Ana);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x1000,  Unk  ),
		Span::new(id, 0x1000, 0x2000,  Ana ),
		Span::new(id, 0x2000, 0x10000, Ana ),
	]);

	// 3. undefine with no coalescing.
	m.undefine(0x2000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x1000,  Unk  ),
		Span::new(id, 0x1000, 0x2000,  Ana ),
		Span::new(id, 0x2000, 0x10000, Unk  ),
	]);

	// 4. undefine with coalescing on both sides.
	m.undefine(0x1000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x10000, Unk  ),
	]);

	// 5. define at the beginning of and the end of existing ones.
	m.define(0x0000, 0x1000, Ana);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x1000,  Ana ),
		Span::new(id, 0x1000, 0x10000, Unk  ),
	]);

	m.define(0xF000, 0x1000, Ana);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x1000,  Ana ),
		Span::new(id, 0x1000, 0xF000,  Unk  ),
		Span::new(id, 0xF000, 0x10000, Ana ),
	]);

	m.define(0x1000, 0x1000, Ana);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x1000,  Ana ),
		Span::new(id, 0x1000, 0x2000,  Ana ),
		Span::new(id, 0x2000, 0xF000,  Unk  ),
		Span::new(id, 0xF000, 0x10000, Ana ),
	]);

	m.define(0xE000, 0x1000, Ana);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x1000,  Ana ),
		Span::new(id, 0x1000, 0x2000,  Ana ),
		Span::new(id, 0x2000, 0xE000,  Unk  ),
		Span::new(id, 0xE000, 0xF000,  Ana ),
		Span::new(id, 0xF000, 0x10000, Ana ),
	]);

	// 6. undefine on ends
	m.undefine(0x0000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x1000,  Unk ),
		Span::new(id, 0x1000, 0x2000,  Ana ),
		Span::new(id, 0x2000, 0xE000,  Unk  ),
		Span::new(id, 0xE000, 0xF000,  Ana ),
		Span::new(id, 0xF000, 0x10000, Ana ),
	]);

	m.undefine(0xF000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x1000,  Unk ),
		Span::new(id, 0x1000, 0x2000,  Ana ),
		Span::new(id, 0x2000, 0xE000,  Unk  ),
		Span::new(id, 0xE000, 0xF000,  Ana ),
		Span::new(id, 0xF000, 0x10000, Unk ),
	]);

	// 7. one-sided coalescing
	m.define(0x2000, 0xC000, Ana);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x1000,  Unk ),
		Span::new(id, 0x1000, 0x2000,  Ana ),
		Span::new(id, 0x2000, 0xE000,  Ana  ),
		Span::new(id, 0xE000, 0xF000,  Ana ),
		Span::new(id, 0xF000, 0x10000, Unk ),
	]);

	m.undefine(0x1000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x2000,  Unk ),
		Span::new(id, 0x2000, 0xE000,  Ana  ),
		Span::new(id, 0xE000, 0xF000,  Ana ),
		Span::new(id, 0xF000, 0x10000, Unk ),
	]);

	m.undefine(0xE000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x2000,  Unk ),
		Span::new(id, 0x2000, 0xE000,  Ana  ),
		Span::new(id, 0xE000, 0x10000, Unk ),
	]);

	// 8. and just for good measure
	m.undefine(0x2000);

	assert_eq!(m.iter().collect::<Vec<_>>(), &[
		Span::new(id, 0x0000, 0x10000,  Unk ),
	]);
}

#[test]
fn span_map_iter() {
	use SpanKind::*;

	let id = SegId(0);
	let mut m = SpanMap::new(id, 2usize.pow(16));
	m.define(0x0000, 0x1000, Ana);
	m.define(0xF000, 0x1000, Ana);
	m.define(0x1000, 0x1000, Ana);
	m.define(0xE000, 0x1000, Ana);

	let spans = &[
		Span::new(id, 0x0000, 0x1000,  Ana ),
		Span::new(id, 0x1000, 0x2000,  Ana ),
		Span::new(id, 0x2000, 0xE000,  Unk  ),
		Span::new(id, 0xE000, 0xF000,  Ana ),
		Span::new(id, 0xF000, 0x10000, Ana ),
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