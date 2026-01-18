// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Structs, Strings and Enums to support trace logging of radeco
//!
//! To enable logging support, compile the library with
//! the `trace_lgo` features, i.e.
//! `cargo build --features trace_log`
//!
//! The consumer should also import `env_logger` and `log` crates
//! and initialize `env_logger` though:
//! `env_logger::init()`
//!
//! Check minidec/main.rs for an example of the same

use std::fmt::{Debug, Display};

pub enum Event<'a, T: 'a + Debug> {
    /// external -> internal
    SSAInsertNode(&'a T),
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

impl<'a, T: Debug> Display for Event<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Event::SSAInsertNode(i) => write!(f, "ssa_insert_node|{i:?}"),
            Event::SSARemoveNode(i) => write!(f, "ssa_remove_node|{i:?}"),
            Event::SSAReplaceNode(i, j) => write!(f, "ssa_replace|{i:?}|{j:?}"),
            Event::SSAInsertEdge(i, j) => write!(f, "ssa_insert_edge|{i:?}|{j:?}"),
            Event::SSARemoveEdge(i, j) => write!(f, "ssa_remove_edge|{i:?}|{j:?}"),
            Event::SSAUpdateEdge(i, j) => write!(f, "ssa_update_edge|{i:?}|{j:?}"),
            Event::SSAMarkNode(i) => write!(f, "ssa_mark_node|{i:?}"),
            Event::SSAClearMark(i) => write!(f, "ssa_clear_mark|{i:?}"),
            Event::SSAQueryExternal(i) => write!(f, "ssa_query_external|{i:?}"),
            Event::SSAQueryInternal(i) => write!(f, "ssa_query_internal|{i:?}"),
        }
    }
}

#[macro_export]
macro_rules! radeco_trace {
    ($t: expr) => ({
        if cfg!(feature = "trace_log") {
            #[cfg(feature="trace_log")]
            debug!("{}", $t.to_string());
        }
    });
    ($fmt:expr, $($arg:tt)*) => ({
        if cfg!(feature = "trace_log") {
            #[cfg(feature="trace_log")]
            debug!("{}", format_args!($fmt, $($arg)*));
        }
    });
}

#[macro_export]
macro_rules! radeco_warn {
    ($t: expr) => ({
        if cfg!(feature = "trace_log") {
            #[cfg(feature="trace_log")]
            warn!("{}", $t.to_string());
        } else {
            let _ = &$t;
        }
    });
    ($fmt:expr, $($arg:tt)*) => ({
        if cfg!(feature = "trace_log") {
            #[cfg(feature="trace_log")]
            warn!("{}", format_args!($fmt, $($arg)*));
        }
    });
}

#[macro_export]
macro_rules! radeco_err {
    ($t: expr) => ({
        if cfg!(feature = "trace_log") {
            #[cfg(feature="trace_log")]
            warn!("{}", $t.to_string());
        } else {
            let _ = &$t;
        }
    });
    ($fmt:expr, $($arg:tt)*) => ({
        if cfg!(feature = "trace_log") {
            #[cfg(feature="trace_log")]
            error!("{}", format_args!($fmt, $($arg)*));
        }
    });
}
