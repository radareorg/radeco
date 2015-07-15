use std::ops::Range;
use frontend::structs::LRegInfo;

type RegOffset = u64;
type RegIndex = u16;

pub struct RegInfo {
	ranges: Vec<(RegOffset, Range<usize>)>,
	compressed: Vec<RegIndex>
}

// struct RegisterSlice {
// 
// }

impl RegInfo {
	pub fn new(reg_info: &LRegInfo) -> RegInfo {
		let mut info = RegInfo {
			ranges:     Vec::new(),
			compressed: Vec::new()
		};

		#[derive(Clone, Copy)]
		enum EventKind { Begin, End };
		struct Event(RegOffset, RegIndex, EventKind);

		let mut events: Vec<Event> = Vec::new();
		for (i, reg) in reg_info.reg_info.iter().enumerate() {
			events.push(Event(reg.offset,                         i as RegIndex, EventKind::Begin));
			events.push(Event(reg.offset + reg.size as RegOffset, i as RegIndex, EventKind::End));
		}

		events.sort_by(|a, b| a.0.cmp(&b.0));
		let mut i = events.iter().peekable();

		let mut active: Vec<RegIndex> = Vec::new();
		while let Option::Some(event) = i.next() {
			let &Event(offset, index, eventtype) = event;
			match eventtype {
				EventKind::Begin => active.push(index as RegIndex),
				EventKind::End   => active.retain(|&active_index| active_index != (index as RegIndex))
			}
			if match i.peek() {
				Option::None => true,
				Option::Some(&&Event(peek_offset, _, _)) => offset != peek_offset
			} {
				let start = info.compressed.len();
				info.compressed.extend(active);
				let end = info.compressed.len();
				info.ranges.push((offset, Range{start: start, end: end}));
				active = Vec::new();
			}
		}
		info
	}
}

//pub struct RegFileMap<T>;
