// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Implements `InternalIndex` used for `DomTree`.

use petgraph::graph::NodeIndex;
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::hash::{Hash, Hasher};
use std::ops::Index;

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

impl Eq for InternalIndex {}

impl Index<InternalIndex> for Vec<InternalIndex> {
    type Output = InternalIndex;
    fn index(&self, index: InternalIndex) -> &InternalIndex {
        &self[index.index]
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
