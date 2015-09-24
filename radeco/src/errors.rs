use std::io;
use std::fmt;
use rustc_serialize::json;

#[derive(Debug)]
pub enum ArgError {
	DecodeError(json::DecoderError),
	InvalidArgument(String),
	IoError(io::Error),
	InvalidSource(String),
	NoSource,
	MultipleSources,
	MissingArgument(String),
}

impl fmt::Display for ArgError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			ArgError::IoError(ref err) => write!(f, "{}", err),
			ArgError::DecodeError(ref err) => write!(f, "{}", err),
			ArgError::InvalidArgument(ref err) => write!(f, "Invalid Argument: {}", err),
			ArgError::NoSource => write!(f, "{}", "No source to run on. Please input binary or raw esil"),
			ArgError::MultipleSources => write!(f, "{}", "Cannot run radeco on multiple sources"),
			ArgError::MissingArgument(ref err) => write!(f, "Missing Argument: {}", err),
			ArgError::InvalidSource(ref err) => write!(f, "{}", err),
		}
	}
}

impl From<io::Error> for ArgError {
	fn from(e: io::Error) -> ArgError {
		ArgError::IoError(e)
	}
}

impl From<json::DecoderError> for ArgError {
	fn from(e: json::DecoderError) -> ArgError {
		ArgError::DecodeError(e)
	}
}
