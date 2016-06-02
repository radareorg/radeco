// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Structs, Strings and Enums to support trace logging of radeco

use std::fmt::Debug;
use log::{self, LogLevel, LogMetadata, LogRecord, SetLoggerError};
use std::fs::OpenOptions;
use std::path::Path;
use std::io::Write;

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
        match *self {
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
        }
    }

}

#[macro_export]
macro_rules! radeco_trace {
	($t: expr) => ({
		if cfg!(feature = "trace_log") {
			trace!("{}", $t.to_string());
		}
	});
    ($fmt:expr, $($arg:tt)*) => ({
        if cfg!(feature = "trace_log") {
            trace!("{}", format_args!($fmt, $($arg)*));
        }
    });
}

macro_rules! radeco_warn {
	($t: expr) => ({
		if cfg!(feature = "trace_log") {
			warn!("{}", $t.to_string());
		}
	});
    ($fmt:expr, $($arg:tt)*) => ({
        if cfg!(feature = "trace_log") {
            warn!("{}", format_args!($fmt, $($arg)*));
        }
    });
}


/// Support for consumers to use logger.
#[derive(Debug)]
pub struct RadecoLogger<T: AsRef<Path>> {
    f: Option<T>,
    level: LogLevel,
}

impl<T: AsRef<Path> + Send + Sync> log::Log for RadecoLogger<T> {
    fn enabled(&self, metadata: &LogMetadata) -> bool {
        metadata.level() <= self.level
    }

    fn log(&self, record: &LogRecord) {
        if self.enabled(record.metadata()) {
            let fmt = format!("{}|{}:{} -> {}",
                              record.level(),
                              record.location().file(),
                              record.location().line(),
                              record.args());
            if let Some(ref fname) = self.f {
                let mut f = OpenOptions::new()
                                .append(true)
                                .open(fname.as_ref())
                                .expect("Unable to open log file");
                f.write_all(fmt.as_bytes()).expect("Write failed");
            } else {
                println!("{}", fmt);
            }
        }
    }
}

pub fn logger_init<T>(f: Option<T>, level: Option<LogLevel>) -> Result<(), SetLoggerError>
where T: 'static + AsRef<Path> + Send + Sync + Clone {
    log::set_logger(|max_log_level| {
        max_log_level.set(level.as_ref().unwrap_or(&LogLevel::Trace).to_log_level_filter());
        Box::new(RadecoLogger {
            f: f,
            level: level.unwrap_or(LogLevel::Trace),
        })
    })
}

macro_rules! enable_logging {
    () => (utils::logger::logger_init::<String>(None, None).expect("Logger Init Failed"));
    ($f: expr) => (logger_init(Some($f), None).expect("Logger Init Failed"));
    ($f: expr, $l: expr) => (logger_init($f, $l).expect("Logger Init Failed"));
    (stdout $l: expr) => (logger_init::<String>(None, Some($l)).expect("Logger Init Failed"));
}
