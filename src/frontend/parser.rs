//! Implements parser to convert from ESIL to RadecoIR.

use num::traits::Num;

use std::collections::HashMap;
use std::cmp;
use regex::Regex;

use super::{MInst, MVal, MOpcode, MValType, Address, MArity, MRegInfo, MAddr};
use super::structs::{LOpInfo, LAliasInfo, LRegInfo, LRegProfile, LFlagInfo};

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

// Convert hex string ("0xbadc0de") to decimal representation.
macro_rules! hex_to_i {
    ( $x:expr ) => {
        Num::from_str_radix($x.trim_left_matches("0x"), 16)
    };
}

#[derive(Debug)]
pub enum ParseError {
    InvalidEsil,
    InvalidMOperator,
    InsufficientOperands,
}

#[allow(dead_code)]
pub struct Parser<'a> {
    stack:        Vec<MVal>,
    insts:        Vec<MInst>,
    opset:        HashMap<&'a str, MOpcode>,
    regset:       HashMap<String, LRegProfile>,
    alias_info:   HashMap<String, LAliasInfo>,
    flags:        HashMap<u64, LFlagInfo>,
    default_size: u8,
    tmp_prefix:   String,
    arch:         String,
    addr:         Address,
    opinfo:       Option<LOpInfo>,
    tmp_index:    u64,
    last_assgn:   MVal,
}

// Struct used to configure the Parser. If `None` is passed to any of the fields, then the default
// values are set.
pub struct ParserConfig<'a> {
    arch:         Option<String>,
    default_size: Option<u8>,
    tmp_prefix:   Option<String>,
    init_opset:   Option<fn() -> HashMap<&'a str, MOpcode>>,
    regset:       Option<HashMap<String, LRegProfile>>,
    alias_info:   Option<HashMap<String, LAliasInfo>>,
    flags:        Option<HashMap<u64, LFlagInfo>>,
}

impl<'a> Default for ParserConfig<'a> {
    fn default() -> Self {
        ParserConfig {
            arch:         Some("x86_64".to_string()),
            default_size: Some(64),
            tmp_prefix:   Some("tmp".to_string()),
            init_opset:   Some(map_esil_to_opset),
            regset:       Some(HashMap::new()),
            alias_info:   Some(HashMap::new()),
            flags:        Some(HashMap::new()),
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(config: Option<ParserConfig<'a>>) -> Parser<'a> {
        let config = config.unwrap_or_default();
        let arch = config.arch.unwrap_or("x86_64".to_string());
        let default_size = config.default_size.unwrap_or(64);
        let tmp_prefix = config.tmp_prefix.unwrap_or("tmp".to_string());
        let init_opset = config.init_opset.unwrap_or(map_esil_to_opset);
        let regset = config.regset.unwrap_or(HashMap::new());
        let alias_info = config.alias_info.unwrap_or(HashMap::new());
        let flags = config.flags.unwrap_or(HashMap::new());
        let val = MVal::null();

        Parser { 
            stack:        Vec::new(),
            insts:        Vec::new(),
            opset:        init_opset(),
            regset:       regset,
            default_size: default_size,
            arch:         arch,
            tmp_prefix:   tmp_prefix,
            addr:         0,
            opinfo:       None,
            alias_info:   alias_info,
            flags:        flags,
            tmp_index:    0,
            last_assgn:  val,
        }
    }

    pub fn set_register_profile(&mut self, reg_info: &LRegInfo) {
        self.regset = HashMap::new();
        let mut tmp: HashMap<String, LAliasInfo> = HashMap::new();
        for alias in reg_info.alias_info.iter() {
            let a = alias.clone();
            tmp.insert(a.reg.clone(), a.clone());
        }

        for reg in reg_info.reg_info.iter() {
            let r = reg.clone();
            self.regset.insert(r.name.clone(), r.clone());
        }

        self.alias_info = tmp.clone();
    }

    pub fn set_flags(&mut self, flags: &Vec<LFlagInfo>) {
        for f in flags.iter() {
            self.flags.insert(f.offset.clone(), f.clone());
        }
    }

    pub fn new_mreg_info(&self, info: LRegProfile) -> MRegInfo {
        MRegInfo {
            reg_type: info.type_str,
            offset:   info.offset,
            reg:      info.name,
            size:     info.size,
            alias:    String::new(),
        }
    }

    pub fn parse_opinfo(&mut self, opinfo: &LOpInfo) -> Result<(), ParseError> {
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
                None     => MOpcode::OpInvalid,
            };

            if op != MOpcode::OpInvalid {
                try!(self.add_inst(op));
                continue;
            }

            // If it contains atleast one alpha, it cannot be an operator.
            let re = Regex::new("[a-zA-Z]").unwrap();
            if re.is_match(&*token) {
                let mut val_type = MValType::Unknown;
                let mut val: Option<u64> = None;
                let mut size: u8 = self.default_size;
                let mut reg_info: Option<MRegInfo> = None;
                if let Some(r) = self.regset.get(&*token) {
                    val_type = MValType::Register;
                    let mut reg_info_ = self.new_mreg_info(r.clone());
                    let alias = self.alias_info.get(&reg_info_.reg)
                        .map(|x| x.role_str.clone())
                        .unwrap_or_default();
                    reg_info_.alias = alias;
                    reg_info = Some(reg_info_.clone());
                    size = r.size; 
                } else if let Ok::<i64, _>(v) = hex_to_i!(token) { // <u64>? will it be able to deal with negative numbers?
                    val = Some(v as u64);
                } else if let Some('%') = token.chars().nth(0) {
                    val_type = MValType::Internal
                } else {
                    panic!("Proper error handling here");
                }
                let v = if let Some(cv) = val {
                    self.constant_value(cv)
                } else {
                    MVal::new(String::from(token), size, val_type, 0, reg_info)
                };
                self.stack.push(v);
                continue;
            }

            // Handle constants.
            if let Ok(num) = token.parse::<i64>() { // <u64>? will it be able to deal with negative numbers?
                let v = self.constant_value(num as u64);
                self.stack.push(v);
                continue;
            }

            // Deal with normal 'composite' instructions.
            if token.char_indices().last().unwrap().1 != ']' {
                let mut dst: MVal;
                if let Some(x) = self.stack.last() {
                    dst = x.clone();
                } else {
                    return Err(ParseError::InsufficientOperands);
                }
                let re = Regex::new(r"^(.|..)=$").unwrap();
                let t = re.captures(&*token).unwrap().at(1).unwrap_or("");
                if t.len() == 0 {
                    return Err(ParseError::InvalidMOperator);
                }
                let op = match self.opset.get(t) {
                    Some(op) => op.clone(),
                    None     => return Err(ParseError::InvalidMOperator),
                };

                try!(self.add_inst(op));
                self.stack.push(dst);
                try!(self.add_inst(MOpcode::OpEq));
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
                _  => access_size.parse::<u8>().unwrap() * 8,
            };

            try!(self.add_inst(MOpcode::OpLoad));
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
                try!(self.add_inst(MOpcode::OpEq));
                continue;
            }

            // 'poke' with another operation. (<op>=[n])
            let o = match self.opset.get(has_op) {
                Some(x) => x.clone(),
                // Return with error
                None => return Err(ParseError::InvalidMOperator),
            };
            try!(self.add_inst(o));
            // Reassignment.
            self.stack.push(tmp_dst1);
            try!(self.add_inst(MOpcode::OpEq));
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

    pub fn emit_insts(&mut self) -> Vec<MInst> {
        let mut res: Vec<MInst> = Vec::new();
        {
            let mut insts_iter = self.insts.iter();
            while let Some(inst) = insts_iter.next() {
                if inst.opcode != MOpcode::OpIf && inst.opcode != MOpcode::OpJmp {
                    res.push(inst.clone());
                    continue;
                }

                if inst.opcode == MOpcode::OpJmp {
                    while let Some(_inst) = insts_iter.next() {
                        if _inst.addr.val != inst.addr.val {
                            res.push(inst.clone());
                            res.push(_inst.clone());
                            break;
                        }
                        res.push(_inst.clone());
                    }
                    continue;
                }

                let mut jmp_inst = None;
                while let Some(_inst) = insts_iter.next() {
                    if _inst.opcode == MOpcode::OpCl {
                        break;
                    }
                    if _inst.opcode != MOpcode::OpJmp {
                        res.push(_inst.clone());
                        continue;
                    }

                    jmp_inst = Some(
                        MInst::new(MOpcode::OpCJmp, MVal::null(),
                        inst.operand_1.clone(),
                        _inst.clone().operand_1, Some(inst.addr.clone()))
                        );
                }

                res.push(jmp_inst.unwrap());
            }
        }

        self.insts = res;
        return (self).insts.clone();
    }

    fn get_tmp_register(&mut self, mut size: u8) -> MVal {
        self.tmp_index += 1;
        if size == 0 {
            size = self.default_size;
        }
        MVal::tmp(self.tmp_index, size)
    }

    // Convert a lower field width to higher field width.
    fn add_widen_inst(&mut self, op: &mut MVal, size: u8) {
        if op.size >= size {
            return;
        }
        let dst = self.get_tmp_register(size);
        let operator = MOpcode::OpWiden(size);
        let addr = MAddr::new(self.addr);
        let inst = MInst::new(operator, dst.clone(), op.clone(), MVal::null(), Some(addr));
        self.insts.push(inst.clone());
        *op = dst;
    }

    // Convert a higher field width to lower field width.
    fn add_narrow_inst(&mut self, op: &mut MVal, size: u8) {
        if op.size <= size {
            return;
        }
        let dst = self.get_tmp_register(size);
        let operator = MOpcode::OpNarrow(size);
        let addr = MAddr::new(self.addr);
        let inst = MInst::new(operator, dst.clone(), op.clone(), MVal::null(), Some(addr));
        self.insts.push(inst.clone());
        *op = dst;
    }

    fn add_assign_inst(&mut self, op: MOpcode) -> Result<(), ParseError> {
        let     dst = try!(self.stack.pop().ok_or(ParseError::InsufficientOperands));
        let mut op1 = try!(self.get_param().ok_or(ParseError::InsufficientOperands));

        // Check the alias of dst. If it is the instruction pointer, the assignment should be a
        // OpJmp rather than a OpEq.
        if dst.reg_info.clone().unwrap_or_default().alias == "pc" {
            let mut op = MOpcode::OpJmp;
            if let Some(ref info) = self.opinfo {
                let optype = info.clone().optype.unwrap_or("".to_string());
                if optype == "call" {
                    op = MOpcode::OpCall;
                }
            }

            let addr = MAddr::new(self.addr);
            let inst = MInst::new(op, MVal::null(), op1, MVal::null(), Some(addr));
            self.insts.push(inst.clone());
            return Ok(());
        }

        // If the dst is a register. Update the last_assgn information.
        if dst.val_type == MValType::Register {
            self.last_assgn = dst.clone();
        }

        if dst.size == op1.size {
            let addr = MAddr::new(self.addr);
            let inst = MInst::new(op, dst.clone(), op1, MVal::null(), Some(addr));
            self.insts.push(inst.clone());
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

    fn add_inst(&mut self, op: MOpcode) -> Result<(), ParseError> {
        // Handle all the special cases.
        match op {
            MOpcode::OpCl => {
                let null = MVal::null();
                let addr = MAddr::new(self.addr);
                let inst = MInst::new(op, null.clone(), null.clone(), null.clone(), Some(addr));
                self.insts.push(inst.clone());
                return Ok(());
            },
            MOpcode::OpInc |
            MOpcode::OpDec => {
                let _op = match op {
                    MOpcode::OpInc => MOpcode::OpAdd,
                    MOpcode::OpDec => MOpcode::OpSub,
                    _              => unreachable!(),
                };
                let mut top = self.stack.len();
                top -= 2;
                let v = self.constant_value(1);
                self.stack.insert(top, v);
                try!(self.add_inst(_op));
                return Ok(());
            },
            MOpcode::OpEq => {
                return self.add_assign_inst(op);
            },
            _ => { },
        }

        let mut op2 = match self.get_param() {
            Some(ele) => ele,
            None      => return Err(ParseError::InsufficientOperands),
        };

        let mut op1 = MVal::null();
        if op.arity() == MArity::Binary {
            op1 = match self.get_param() {
                Some(ele) => ele,
                None      => return Err(ParseError::InsufficientOperands),
            };
        }

        if op == MOpcode::OpIf {
            let addr = MAddr::new(self.addr);
            let inst = MInst::new(op, MVal::null(), op2, op1, Some(addr));
            self.insts.push(inst);
            return Ok(());
        }

        let mut dst_size: u8;
        let mut dst: MVal;
        dst_size = cmp::max(op1.size, op2.size);
        dst = self.get_tmp_register(dst_size);

        // Add a check to see if dst, op1 and op2 have the same size.
        // If they do not, cast it. op2 is never 'Null'.
        assert!(op2.val_type != MValType::Null);

        if op.arity() == MArity::Binary {
            if op1.size > op2.size {
                dst_size = op1.size;
                self.add_widen_inst(&mut op2, op1.size);
            } else if op2.size > op1.size {
                dst_size = op2.size;
                self.add_widen_inst(&mut op1, op2.size);
            }
        }

        dst.size = dst_size;
        
        // If it is a compare instruction, then the flags must be updated.
        if op == MOpcode::OpCmp {
            self.last_assgn = dst.clone();
        }

        let addr = MAddr::new(self.addr);
        let inst = MInst::new(op, dst.clone(), op2, op1, Some(addr));
        self.insts.push(inst);
        self.stack.push(dst);

        Ok(())
    }

    // correspons to r_anal_esil_get_parm
    fn get_param(&mut self) -> Option<MVal> {
        if let Some(mv) = self.stack.pop() {
            Some(match mv.val_type {
                MValType::Internal => {
                    let addr = MAddr::new(self.addr);
                    match mv.name.chars().nth(1).unwrap_or('\0') {
                        '%' => {
                            let addr = self.addr;
                            self.constant_value(addr)
                        },
                        'z' => {
                            let dst = self.get_tmp_register(1);
                            let inst = MInst::new(MOpcode::OpCmp, dst.clone(), self.last_assgn.clone(), self.constant_value(0), Some(addr));
                            self.insts.push(inst);
                            dst
                        },
                        //'b' => _ // OpIFBorrow(u8),
                        //'c' => _ // OpIFCarry(u8),
                        //'o' => _ // OpIFOverflow,
                        //'p' => _ // OpIFParity,
                        //'r' => _ // OpIFRegSize,
                        //'s' => _ // OpIFSign,
                        _ => mv
                    }
                },
                _ => mv
            })
        } else {
            None
        }
    }

    fn constant_value(&mut self, num: u64) -> MVal {
        let op = MOpcode::OpConst(num);
        let size = self.default_size;
        let dst = self.get_tmp_register(size);
        let addr = MAddr::new(self.addr);
        let inst = MInst::new(op, dst.clone(), MVal::null(), MVal::null(), Some(addr));
        self.insts.push(inst);
        dst
    }
}

fn map_esil_to_opset() -> HashMap<&'static str, MOpcode> {
    // Make a map from esil string to struct MOperator.
    // (operator: &str, op: MOperator).
    // Possible Optimization:  Move to compile-time generation ?
    hash![
        ("==" , MOpcode::OpCmp),
        ("<"  , MOpcode::OpLt),
        (">"  , MOpcode::OpGt),
        ("<=" , MOpcode::OpGteq),
        (">=" , MOpcode::OpLteq),
        ("<<" , MOpcode::OpLsl),
        (">>" , MOpcode::OpLsr),
        ("&"  , MOpcode::OpAnd),
        ("|"  , MOpcode::OpOr),
        ("="  , MOpcode::OpEq),
        ("*"  , MOpcode::OpMul),
        ("^"  , MOpcode::OpXor),
        ("+"  , MOpcode::OpAdd),
        ("-"  , MOpcode::OpSub),
        ("/"  , MOpcode::OpDiv),
        ("%"  , MOpcode::OpMod),
        ("?{" , MOpcode::OpIf),
        ("!"  , MOpcode::OpNot),
        ("--" , MOpcode::OpDec),
        ("++" , MOpcode::OpInc),
        ("}"  , MOpcode::OpCl)
     ]
}
