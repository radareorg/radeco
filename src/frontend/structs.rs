//! Module provides basic structs which are used for json encoding and decoding.
use rustc_serialize::{Decodable, Decoder};
#[derive(RustcEncodable, Debug, Clone)]
pub struct OpInfo {
    pub esil:   Option<String>,
    pub offset: Option<u64>,
    pub opcode: Option<String>,
    pub optype: Option<String>,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct FunctionInfo {
    pub addr: Option<u64>,
    pub name: Option<String>,
    pub ops:  Option<Vec<OpInfo>>,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct LRegInfo {
    pub alias_info: Vec<AliasInfo>,
    pub reg_info: Vec<RegProfile>,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct AliasInfo {
    pub reg: String,
    pub role: u64,
    pub role_str: String,
}

#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct RegProfile {
    pub name: String,
    pub offset: u64,
    pub size: u8,
    pub type_str: String,
}

impl Decodable for OpInfo {
    fn decode<D: Decoder>(d: &mut D) -> Result<OpInfo, D::Error> {
        d.read_struct("root", 0, |d_| {
            let op = OpInfo {
                esil:   d_.read_struct_field("esil", 0, |d| Decodable::decode(d)).ok(),
                offset: d_.read_struct_field("offset", 0, |d| Decodable::decode(d)).ok(),
                opcode: d_.read_struct_field("opcode", 0, |d| Decodable::decode(d)).ok(),
                optype: d_.read_struct_field("type", 0, |d| Decodable::decode(d)).ok(),
            };

            Ok(op)
        })
    }
}
