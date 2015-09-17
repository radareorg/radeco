// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Basic structs which are used for json encoding and decoding.

use rustc_serialize::{Decodable, Decoder};
#[derive(RustcEncodable, Debug, Clone)]
pub struct LOpInfo {
	pub esil:   Option<String>,
	pub offset: Option<u64>,
	pub opcode: Option<String>,
	pub optype: Option<String>,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct LFunctionInfo {
	pub addr: Option<u64>,
	pub name: Option<String>,
	pub ops:  Option<Vec<LOpInfo>>,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct LRegInfo {
	pub alias_info: Vec<LAliasInfo>,
	pub reg_info:   Vec<LRegProfile>,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone, Default)]
pub struct LAliasInfo {
	pub reg:      String,
	pub role:     u64,
	pub role_str: String,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone, Default)]
pub struct LRegProfile {
	pub name:     String,
	pub offset:   usize,
	pub size:     usize,
	pub type_str: String,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone, Default)]
pub struct LFlagInfo {
	pub offset: u64,
	pub name:   String,
	pub size:   u64,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone, Default)]
pub struct LBinInfo {
	pub core: Option<LCoreInfo>,
	pub bin: Option<LBin>,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone, Default)]
pub struct LCoreInfo {
	pub file: Option<String>,
	pub size: Option<usize>,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone, Default)]
pub struct LBin {
	pub arch: Option<String>,
}

impl Decodable for LOpInfo {
	fn decode<D: Decoder>(d: &mut D) -> Result<LOpInfo, D::Error> {
		d.read_struct("root", 0, |d_| {
			let op = LOpInfo {
				esil:   d_.read_struct_field("esil", 0, |d| Decodable::decode(d)).ok(),
				offset: d_.read_struct_field("offset", 0, |d| Decodable::decode(d)).ok(),
				opcode: d_.read_struct_field("opcode", 0, |d| Decodable::decode(d)).ok(),
				optype: d_.read_struct_field("type", 0, |d| Decodable::decode(d)).ok(),
			};

			Ok(op)
		})
	}
}
