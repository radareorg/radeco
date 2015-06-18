// TODO: Add License information.

//! Module to parse ESIL strings and convert them into the IR.
//!
//! ESIL (Evaluable Strings Intermediate Language) is the IL used by radare2.
//!
//! For a complete documentation of ESIL please check 
//!  [wiki](https://github.com/radare/radare2/wiki/ESIL).
//!
//! # Details
//!
//! The `Parser` struct provides methods needed to convert a valid ESIL string
//! into the IR. `Parser::parse()` parses the ESIL string and returns an `Err`
//! if the ESIL string is Invalid.
//!
//! `Parser` also provides `Parser::emit_insts()` to extract the `Instructions` 
//! it generates. Calling `Parser::parse()` several times will add more instructions.
//! 
//! # Example
//!
//! ```
//! use radeco::frontend::esil;
//! let esil = "eax,ebx,^=";
//! let mut p = esil::Parser::new();
//! p.parse(esil, None);
//! for inst in &p.emit_insts() {
//!     println!("{}", inst);
//! }
//! ```

extern crate num;
use self::num::traits::Num;

use std::collections::HashMap;
use std::cmp;
use regex::Regex;

use super::{Instruction, Value, Opcode, Location, Address, Arity};
use super::structs::{OpInfo};

// Macro to return a new hash given (key, value) tuples.
// Example: hash![("foo", "bar"), ("bar", "baz")]
macro_rules! hash {
    ( $( ($x:expr, $y:expr) ),* ) => {
        {
            let mut temp_hash = HashMap::new();
            $(
                temp_hash.insert($x, $y);
             )*
                temp_hash
        }
    };
}

macro_rules! hex_to_i {
    ( $x:expr ) => {
        Num::from_str_radix($x.trim_left_matches("0x"), 16)
    };
}

#[derive(Debug)]
pub enum ParseError {
    InvalidEsil,
    InvalidOperator,
    InsufficientOperands,
}

fn map_esil_to_opset() -> HashMap<&'static str, Opcode> {
    // Make a map from esil string to struct Operator.
    // (operator: &str, op: Operator).
    // Possible Optimization:  Move to compile-time generation ?
    hash![
        ("==" , Opcode::OpCmp),
        ("<"  , Opcode::OpLt),
        (">"  , Opcode::OpGt),
        ("<=" , Opcode::OpGteq),
        (">=" , Opcode::OpLteq),
        ("<<" , Opcode::OpLsl),
        (">>" , Opcode::OpLsr),
        ("&"  , Opcode::OpAnd),
        ("|"  , Opcode::OpOr),
        ("="  , Opcode::OpEq),
        ("*"  , Opcode::OpMul),
        ("^"  , Opcode::OpXor),
        ("+"  , Opcode::OpAdd),
        ("-"  , Opcode::OpSub),
        ("/"  , Opcode::OpDiv),
        ("%"  , Opcode::OpMod),
        ("?{" , Opcode::OpIf),
        ("!"  , Opcode::OpNot),
        ("--" , Opcode::OpDec),
        ("++" , Opcode::OpInc),
        ("}"  , Opcode::OpCl)
    ]
}

fn init_regset() -> HashMap<&'static str, u8> {
    // Use from sdb later, probably a better option.
    hash![
        ("rax", 64),
        ("rbx", 64),
        ("rcx", 64),
        ("rdx", 64),
        ("rsp", 64),
        ("rbp", 64),
        ("rsi", 64),
        ("rdi", 64),
        ("rip", 64),
        ("zf",   1)
    ]
}

pub struct Parser<'a> {
    stack:        Vec<Value>,
    insts:        Vec<Instruction>,
    opset:        HashMap<&'a str, Opcode>,
    regset:       HashMap<&'a str, u8>,
    tmp_index:    u64,
    default_size: u8,
    ip:           String,
    tmp_prefix:   String,
    arch:         String,
    addr:         Address,
    opinfo:       Option<OpInfo>,
}

// Struct used to configure the Parser. If `None` is passed to any of the fields, then the default
// values are set.
pub struct ParserConfig<'a> {
    arch:         Option<String>,
    default_size: Option<u8>,
    ip:           Option<String>,
    tmp_prefix:   Option<String>,
    init_opset:   Option<fn() -> HashMap<&'a str, Opcode>>,
    init_regset:  Option<fn() -> HashMap<&'a str, u8>>,
}

// Represents the state of the parser.
#[allow(dead_code)]
pub struct ParserState {
    offset:    u64,
    tmp_index: u64,
    opinfo:    Option<OpInfo>,
}

impl<'a> Default for ParserConfig<'a> {
    fn default() -> Self {
        ParserConfig {
            arch: Some("x86_64".to_string()),
            default_size: Some(64),
            ip: Some("rip".to_string()),
            tmp_prefix: Some("tmp".to_string()),
            init_opset: Some(map_esil_to_opset),
            init_regset: Some(init_regset),
        }
    }
}


impl<'a> Parser<'a> {
    pub fn new(config: Option<ParserConfig<'a>>) -> Parser<'a> {
        let config = config.unwrap_or_default();
        let arch = config.arch.unwrap_or("x86_64".to_string());
        let default_size = config.default_size.unwrap_or(64);
        let ip = config.ip.unwrap_or("rip".to_string());
        let tmp_prefix = config.tmp_prefix.unwrap_or("tmp".to_string());
        let init_opset = config.init_opset.unwrap_or(map_esil_to_opset);
        let init_regset = config.init_regset.unwrap_or(init_regset);

        Parser { 
            stack:        Vec::new(),
            insts:        Vec::new(),
            opset:        init_opset(),
            regset:       init_regset(),
            default_size: default_size,
            ip:           ip,
            arch:         arch,
            tmp_prefix:   tmp_prefix,
            tmp_index:    0,
            addr:         0,
            opinfo:       None,
        }
    }

    fn get_tmp_register(&mut self, mut size: u8) -> Value {
        self.tmp_index += 1;
        if size == 0 {
            size = self.default_size;
        }
        Value::tmp(self.tmp_index, size)
    }

    fn add_widen_inst(&mut self, op: &mut Value, size: u8) {
        if op.size >= size {
            return;
        }
        let dst = self.get_tmp_register(size);
        let operator = Opcode::OpWiden;
        self.insts.push(Instruction::new(operator, dst.clone(), op.clone(), Value::constant(size as i64), Some(self.addr)));
        *op = dst;
    }

    fn add_narrow_inst(&mut self, op: &mut Value, size: u8) {
        if op.size <= size {
            return;
        }
        let dst = self.get_tmp_register(size);
        let operator = Opcode::OpNarrow;
        self.insts.push(Instruction::new(operator, dst.clone(), op.clone(), Value::constant(size as i64), Some(self.addr)));
        *op = dst;
    }

    fn add_assign_inst(&mut self, op: Opcode) -> Result<(), ParseError> {
        let dst = match self.stack.pop() {
            Some(ele) => ele,
            None => return Err(ParseError::InsufficientOperands),
        };

        let mut op1 = match self.stack.pop() {
            Some(ele) => ele,
            None => return Err(ParseError::InsufficientOperands),
        };

        // If it is an assignment to the Instruction Pointer, then the Instruction should be a OpJmp
        // rather than an OpEq.
        if dst.name == self.ip {
            let mut op = Opcode::OpJmp;
            if let Some(ref info) = self.opinfo {
                let optype = info.clone().optype.unwrap_or("".to_string());
                if optype == "call" {
                    op = Opcode::OpCall;
                }
            }

            self.insts.push(Instruction::new(op, Value::null(), op1, Value::null(), Some(self.addr)));
            return Ok(());
        }

        if dst.size == op1.size {
            self.insts.push(Instruction::new(op, dst.clone(), op1, Value::null(), Some(self.addr)));
            return Ok(());
        }

        if dst.size > op1.size {
            self.add_widen_inst(&mut op1, dst.size);
        } else {
            self.add_narrow_inst(&mut op1, dst.size);
        }

        // We don't need to use another instruction for assignment. Just replace the dst of the
        // narrow/widen instruction generated.
        self.insts.last_mut().unwrap().dst = dst.clone();
        Ok(())
    }

    fn add_inst(&mut self, op: Opcode) -> Result<(), ParseError> {
        // Handle "}".
        if op == Opcode::OpCl {
            let null = Value::null();
            self.insts.push(Instruction::new(op, null.clone(), null.clone(), null.clone(), Some(self.addr)));
            return Ok(());
        }

        // Assignment operation has to be handled quite differently.
        if op == Opcode::OpEq {
            return self.add_assign_inst(op);
        }

        let mut op2 = match self.stack.pop() {
            Some(ele) => ele,
            None => return Err(ParseError::InsufficientOperands),
        };

        let mut op1 = Value::null();
        if op.to_operator().arity == Arity::Binary {
            op1 = match self.stack.pop() {
                Some(ele) => ele,
                None => return Err(ParseError::InsufficientOperands),
            };
        }

        if op == Opcode::OpIf {
            self.insts.push(Instruction::new(op, Value::null(), op2, op1, Some(self.addr)));
            return Ok(());
        }

        let mut dst_size: u8;
        let mut dst: Value;
        dst_size = cmp::max(op1.size, op2.size);
        dst = self.get_tmp_register(dst_size);

        // Add a check to see if dst, op1 and op2 have the same size.
        // If they do not, cast it. op2 is never 'Null'.
        assert!(op2.location != Location::Null);

        if op.to_operator().arity == Arity::Binary {
            if op1.size > op2.size {
                dst_size = op1.size;
                self.add_widen_inst(&mut op2, op1.size);
            } else if op2.size > op1.size {
                dst_size = op2.size;
                self.add_widen_inst(&mut op1, op2.size);
            }
        }

        dst.size = dst_size;

        self.insts.push(Instruction::new(op, dst.clone(), op2, op1, Some(self.addr)));
        self.stack.push(dst);

        Ok(())
    }

    pub fn parse_opinfo(&mut self, opinfo: &OpInfo) -> Result<(), ParseError> {
        let opinfo = opinfo.clone();
        let esil = opinfo.esil.clone().unwrap_or("".to_string());

        self.addr = match opinfo.offset {
            Some(s) => s,
            None    => self.addr + 1,
        };

        self.opinfo = Some(opinfo);
        self.parse_str(&*esil)
    }

    pub fn parse_str(&mut self, esil: &str) -> Result<(), ParseError> {
        if esil.len() == 0 {
            return Err(ParseError::InvalidEsil);
        }

        let esil: Vec<String> = esil.split(',')
                                    .map(|x| x.to_string()).collect();
        for token in esil {
            let op = match self.opset.get(&*token) {
                Some(op) => op.clone(),
                None => Opcode::OpInvalid,
            };

            if op != Opcode::OpInvalid {
                try!(self.add_inst(op));
                continue;
            }

            // If it contains atleast one alpha, it cannot be an operator.
            let re = Regex::new("[a-zA-Z]").unwrap();
            if re.is_match(&*token) {
                let mut val_type = Location::Unknown;
                let mut val: i64 = 0;
                let mut size: u8 = self.default_size;
                if let Some(r) = self.regset.get(&*token) {
                    val_type = Location::Register;
                    // For now, reg is just a u8.
                    size = *r; 
                } else if let Ok(v) = token.parse::<i64>() {
                    val_type = Location::Constant;
                    val = v;
                } else if let Ok(v) = hex_to_i!(token) {
                    val_type = Location::Constant;
                    val = v;
                }
                let v = Value::new(String::from(token), size, val_type, val, 0);
                self.stack.push(v);
                continue;
            }

            // Handle constants.
            if let Ok(num) = token.parse::<i64>() {
                let val_type = Location::Constant;
                let val = num;
                let size  = self.default_size;
                let name = format!("0x{:x}", num);
                let v = Value::new(name, size, val_type, val, 0);
                self.stack.push(v);
                continue;
            }

            // Deal with normal 'composite' instructions.
            if token.char_indices().last().unwrap().1 != ']' {
                let mut dst: Value;
                if let Some(x) = self.stack.last() {
                    dst = x.clone();
                } else {
                    return Err(ParseError::InsufficientOperands);
                }
                let re = Regex::new(r"^(.|..)=$").unwrap();
                let t = re.captures(&*token).unwrap().at(1).unwrap_or("");
                if t.len() == 0 {
                    return Err(ParseError::InvalidOperator);
                }
                let op = match self.opset.get(t) {
                    Some(op) => op.clone(),
                    None => return Err(ParseError::InvalidOperator),
                };

                try!(self.add_inst(op));
                self.stack.push(dst);
                try!(self.add_inst(Opcode::OpEq));
                continue;
            }

            // Deal with memaccess 'composite' instructions.
            let re = Regex::new(r"^(.|..)?(=)?\[([1248]?)\]$").unwrap();
            let tokens = re.captures(&*token).unwrap();
            let eq = tokens.at(2).unwrap_or("");
            let has_op = tokens.at(1).unwrap_or("");
            let access_size = tokens.at(3).unwrap_or("");
            let access_size = match access_size {
                "" => self.default_size,
                _ => access_size.parse::<u8>().unwrap() * 8,
            };

            try!(self.add_inst(Opcode::OpRef));
            // Set the correct size.
            let mut x = self.stack.pop().unwrap();
            self.add_narrow_inst(&mut x, access_size);
            let tmp_dst1 = x.clone();
            self.stack.push(x);

            // Simple 'peek' ([n])
            if eq.is_empty() {
                continue;
            }

            // Simple 'poke' (=[n])
            if has_op.is_empty() {
                try!(self.add_inst(Opcode::OpEq));
                continue;
            }

            // 'poke' with another operation. (<op>=[n])
            let o = match self.opset.get(has_op) {
                Some(x) => x.clone(),
                // Return with error
                None => return Err(ParseError::InvalidOperator),
            };
            try!(self.add_inst(o));
            // Reassignment.
            self.stack.push(tmp_dst1);
            try!(self.add_inst(Opcode::OpEq));
        }
        Ok(())
    }

    /// Emit-instructions converts certain compound instructions to simpler instructions.
    /// For example, the sequence of instructions,
    /// ```none
    /// if zf            (OpIf)
    ///   jump addr      (OpJmp)
    /// ```
    /// is converted to a single conditional jump as,
    /// ```none
    /// if zf jump addr  (OpCJmp)
    /// ```

    pub fn emit_insts(&mut self) -> Vec<Instruction> {
        let len = self.insts.len();
        let mut res: Vec<Instruction> = Vec::new();
        let mut i = 0;
        while i < len {
            let inst = self.insts[i].clone();
            if inst.opcode != Opcode::OpIf {
                res.push(inst);
                i += 1;
                continue;
            }

            // Note(sushant94): We need to improve this process if we want to retain the side-effects.
            // For example, a x86 call instruction will result the corresponding esil statement to
            // have instructions to set esp and then jump to the required location.
            // We can use these side-effects to determine if it's a 'call' or just a jump.
            // This however requires study into other architectures and their esil too.
            while self.insts[i].opcode != Opcode::OpCl && i < len - 1 {
                i += 1;
                let inst_ = self.insts[i].clone();
                if inst_.opcode == Opcode::OpJmp {
                    let res_inst = 
                        Instruction::new(Opcode::OpCJmp, Value::null(),
                                         inst.operand_1.clone(),
                                         inst_.operand_1, Some(inst.addr));
                    res.push(res_inst);
                }
            }
            i += 1;
        }
        self.insts = res;
        return (self).insts.clone();
    }
}

#[test]
fn testing() {
    let mut p = Parser::new();
    p.parse(String::from("0,0x204db1,rip,+,[1],==,%z,zf,=,%b8,cf,=,%p,pf,=,%s,sf,="), None);
}
