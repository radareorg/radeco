// Copyright (c) 2019, The Radare Project. All rights reserved.
use std::fmt::Debug;

// TODO: Implement proper events for debugging

#[macro_export]
macro_rules! rune_trace {
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
macro_rules! rune_warn {
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

#[macro_export]
macro_rules! rune_err {
    ($t: expr) => ({
        if cfg!(feature = "trace_log") {
            #[cfg(feature="trace_log")]
            warn!("{}", $t.to_string());
        }
    });
    ($fmt:expr, $($arg:tt)*) => ({
        if cfg!(feature = "trace_log") {
            #[cfg(feature="trace_log")]
            error!("{}", format_args!($fmt, $($arg)*));
        }
    });
}
