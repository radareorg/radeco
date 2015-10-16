extern crate log;

use log::{LogRecord, LogLevel, LogMetadata, LogLevelFilter, SetLoggerError};

#[derive(Debug, RustcDecodable, RustcEncodable)]
pub struct RadecoLog {
	log: Vec<RadecoRecord>
}

#[derive(Debug, RustcDecodable, RustcEncodable)]
pub struct RadecoRecord {
	event: String,
	kind: Kind,
	args: Vec<String>,
	file: String,
	line: u32,
}

#[derive(Debug, RustcDecodable, RustcEncodable)]
pub enum Kind {
	SSA,
}

impl RadecoRecord {
	fn new(event: String, kind: Kind, args: Vec<String>, f: String,
	       l: u32) -> RadecoRecord 
	{
		RadecoRecord {
			event: event,
			kind: kind,
			args: args,
			file: f,
			line: l,
		}
	}
}

impl log::Log for RadecoLog {
	fn enabled(&self, metadata: &LogMetadata) -> bool {
		metadata.level() <= LogLevel::Trace
	}

	fn log(&self, record: &LogRecord) {
		if self.enabled(record.metadata()) {
			println!("{} - {} - {}:{}", record.level(), record.args(), record.location().file(), record.location().line());
		}
	}
}

impl RadecoLog {
	pub fn new() -> RadecoLog {
		RadecoLog {
			log: Vec::new(),
		}
	}

	pub fn init() -> Result<(), SetLoggerError> {
		log::set_logger(|max_log_level| {
			max_log_level.set(LogLevelFilter::Trace);
			Box::new(RadecoLog::new())
		})
	}
}
