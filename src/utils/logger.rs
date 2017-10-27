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

use std::fmt::Debug;

#[cfg(feature="trace_log")]
use log::{self, LogLevel, LogMetadata, LogRecord, SetLoggerError};

use std::fs::OpenOptions;
use std::path::Path;
use std::io::Write;

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

impl<'a, T: Debug> ToString for Event<'a, T> {
    fn to_string(&self) -> String {
        match *self {
            Event::SSAInsertNode(i) => {
                format!("{}|{:?}", "ssa_insert_node", i)
            }
            Event::SSARemoveNode(i) => {
                format!("{}|{:?}", "ssa_remove_node", i)
            }
            Event::SSAReplaceNode(i, j) => {
                format!("{}|{:?}|{:?}", "ssa_replace", i, j)
            }
            Event::SSAInsertEdge(i, j) => {
                format!("{}|{:?}|{:?}", "ssa_insert_edge", i, j)
            }
            Event::SSARemoveEdge(i, j) => {
                format!("{}|{:?}|{:?}", "ssa_remove_edge", i, j)
            }
            Event::SSAUpdateEdge(i, j) => {
                format!("{}|{:?}|{:?}", "ssa_update_edge", i, j)
            }
            Event::SSAMarkNode(i) => {
                format!("{}|{:?}", "ssa_mark_node", i)
            }
            Event::SSAClearMark(i) => {
                format!("{}|{:?}", "ssa_clear_mark", i)
            }
            Event::SSAQueryExternal(i) => {
                format!("{}|{:?}", "ssa_query_external", i)
            }
            Event::SSAQueryInternal(i) => {
                format!("{}|{:?}", "ssa_query_internal", i)
            }
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
        }
    });
    ($fmt:expr, $($arg:tt)*) => ({
        if cfg!(feature = "trace_log") {
            #[cfg(feature="trace_log")]
            warn!("{}", format_args!($fmt, $($arg)*));
        }
    });
}
