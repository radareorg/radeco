//! Module provides basic structs which are used for json encoding and decoding.

#[derive(RustcDecodable, RustcEncodable, Debug)]
pub struct OpInfo {
    pub esil: String,
    pub offset: u64,
    pub opcode: String,
}

#[derive(RustcDecodable, RustcEncodable, Debug)]
pub struct FunctionInfo {
    pub addr: u64,
    pub name: String,
    pub ops: Vec<OpInfo>,
}
