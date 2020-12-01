use super::*;

#[test]
fn span_map() {
	let mut m = SpanMap::new(2usize.pow(16));
	m.dump_spans();
	m.redefine(SegOffset(0x1000), SegOffset(0x2000), SpanKind::Code, None);
	m.redefine(SegOffset(0x2000), SegOffset(0x10000), SpanKind::Data, None);
	m.redefine(SegOffset(0x800), SegOffset(0x1800), SpanKind::Unk, None);
	m.dump_spans();
	m.redefine(SegOffset(0), SegOffset(0x800), SpanKind::Code, None);
	m.dump_spans();

	println!("span map iter:");
	for s in m.iter() {
		println!("{}", s.start);
	}

	m.redefine(SegOffset(0), SegOffset(0x10000), SpanKind::Unk, None);
	m.dump_spans();
}