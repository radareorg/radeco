// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Structs, Strings and Enums to support trace logging of radeco

use std::fmt::Debug;

// Proposed log format.
// [
//    {
//        "event": SSAAddNode(e, i),
//        "kind": SSA,
//        "timestamp": 0,
//    },
//    {
//        "event": SSARemoveNode(e, i),
//        "kind": SSA,
//        "timestamp": 1,
//    },
//    ....
// ]

pub enum Event<'a, T: 'a + Debug> {
    /// external -> internal
    SSAInsertNode(&'a T, &'a T),
    /// external
    SSARemoveNode(&'a T),
    /// external, external
    SSAReplaceNode(&'a T, &'a T),
    /// external, external
    SSAInsertEdge(&'a T, &'a T),
    SSARemoveEdge(&'a T, &'a T),
    SSAUpdateEdge(&'a T, &'a T),
    /// external
    SSAMarkNode(&'a T),
    /// external
    SSAClearMark(&'a T),
    /// external
    SSAQueryExternal(&'a T),
    /// internal
    SSAQueryInternal(&'a T),
}

impl<'a, T: Debug> ToString for Event<'a, T> {
    fn to_string(&self) -> String {
        // let timestamp = "";
        let r = match *self {
            Event::SSAInsertNode(ref i, ref j) => {
                format!("{}|{:?}|{:?}", "ssa_insert_node", i, j)
            }
            Event::SSARemoveNode(ref i) => {
                format!("{}|{:?}", "ssa_remove_node", i)
            }
            Event::SSAReplaceNode(ref i, ref j) => {
                format!("{}|{:?}|{:?}", "ssa_replace", i, j)
            }
            Event::SSAInsertEdge(ref i, ref j) => {
                format!("{}|{:?}|{:?}", "ssa_insert_edge", i, j)
            }
            Event::SSARemoveEdge(ref i, ref j) => {
                format!("{}|{:?}|{:?}", "ssa_remove_edge", i, j)
            }
            Event::SSAUpdateEdge(ref i, ref j) => {
                format!("{}|{:?}|{:?}", "ssa_update_edge", i, j)
            }
            Event::SSAMarkNode(ref i) => {
                format!("{}|{:?}", "ssa_mark_node", i)
            }
            Event::SSAClearMark(ref i) => {
                format!("{}|{:?}", "ssa_clear_mark", i)
            }
            Event::SSAQueryExternal(ref i) => {
                format!("{}|{:?}", "ssa_query_external", i)
            }
            Event::SSAQueryInternal(ref i) => {
                format!("{}|{:?}", "ssa_query_internal", i)
            }
        };
        // format!("{}|{}", timestamp, r)
        r
    }

}

macro_rules! radeco_trace {
	($t: expr) => {
		if cfg!(feature = "trace_log") {
			trace!("{}", $t.to_string());
		}
	}
}
