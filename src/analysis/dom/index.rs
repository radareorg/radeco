//! Implements InternalIndex used for DomTree.

use std::cmp::{Eq, PartialEq, Ord, Ordering, PartialOrd};
use std::ops::Index;
use std::hash::{Hash, Hasher};
use petgraph::graph::NodeIndex;

#[derive(Clone, Copy, Debug)]
pub struct InternalIndex {
	index: usize,
	external: NodeIndex,
}

impl InternalIndex {
	pub fn new(index: usize, n: NodeIndex) -> InternalIndex {
		InternalIndex {
			index: index,
			external: n,
		}
	}

	pub fn external(&self) -> NodeIndex {
		self.external
	}

	pub fn index(&self) -> usize {
		self.index
	}
}

///////////////////////////////////////////////////////////////////////////////
//// Trait implementations to ensure InternalIndex 
//// behaves like InternalIndex::index (usize)
///////////////////////////////////////////////////////////////////////////////

impl PartialEq for InternalIndex {
	fn eq(&self, other: &Self) -> bool {
		self.index == other.index
	}
}

impl Hash for InternalIndex {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.index.hash(state);
	}
}

impl Eq for InternalIndex { }

impl Index<InternalIndex> for Vec<InternalIndex> {
	type Output = InternalIndex;
	fn index<'a>(&'a self, _index: InternalIndex) -> &'a InternalIndex {
		&self[_index.index]
	}
}

impl PartialOrd for InternalIndex {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		self.index.partial_cmp(&other.index)
	}
}


impl Ord for InternalIndex {
	fn cmp(&self, other: &Self) -> Ordering {
		self.index.cmp(&other.index)
	}
}

