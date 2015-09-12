use std::io;
use std::fmt;
use rustc_serialize::json;

#[derive(Debug)]
pub enum ReadErr {
	DecodeErr(json::DecoderError),
	IoErr(io::Error),
}

impl fmt::Display for ReadErr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			ReadErr::IoErr(ref err) => write!(f, "{}", err),
			ReadErr::DecodeErr(ref err) => write!(f, "{}", err),
		}
	}
}

impl From<io::Error> for ReadErr {
	fn from(e: io::Error) -> ReadErr {
		ReadErr::IoErr(e)
	}
}

impl From<json::DecoderError> for ReadErr {
	fn from(e: json::DecoderError) -> ReadErr {
		ReadErr::DecodeErr(e)
	}
}
