
use std::iter::FromIterator;
use adi::memory::*;

fn main() {
	let regions = &[
		MemoryRegion::new("DICKS", 50, 1000, true, MemoryRegionKind::Ram),
		MemoryRegion::new("BUTTS", 1000, 2000, true, MemoryRegionKind::Ram),
	];

	let mmap = MemoryMap::new(16, regions);
	println!("{:#?}", mmap);

	println!("{:?}", mmap.get(VAddr(0)));
	println!("{:?}", mmap.get(VAddr(50)));
	println!("{:?}", mmap.get(VAddr(1500)));
	println!("{:?}", mmap.get(VAddr(2000)));

	let config = MemoryConfig::from_iter(&[
		("a", "b"),
	]);

	let config2 = config.derive(&[
		("a", "c"),
		("x", "y"),
	]);

	println!("{:#?}", config);
	println!("{:#?}", config2);

	println!("{:?}", Span::new(SegOffset(10), SegOffset(20), None, SpanKind::Unk));
}

