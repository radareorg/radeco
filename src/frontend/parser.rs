//! Implements parser to convert from ESIL to RadecoIR.

use num::traits::Num;

use std::collections::HashMap;
use std::cmp;
use regex::Regex;

use frontend::{MInst, MVal, MOpcode, MValType, Address, MRegInfo, MAddr};
use frontend::structs::{LOpInfo, LAliasInfo, LRegInfo, LRegProfile, LFlagInfo};
use middle::ir::{WidthSpec}; // move WidthSpec to a different module?

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
#[derive(Clone)]
pub struct Parser<'a> {
	stack:        Vec<MVal>,
	insts:        Vec<MInst>,
	opset:        HashMap<&'a str, MOpcode>,
	opset2:       HashMap<String, EsilOperation>,
	regset:       HashMap<String, LRegProfile>,
	alias_info:   HashMap<String, LAliasInfo>,
	flags:        HashMap<u64, LFlagInfo>,
	default_size: WidthSpec,
	tmp_prefix:   String,
	arch:         String,
	addr:         Address,
	opinfo:       Option<LOpInfo>,
	tmp_index:    u64,
	constants:	  HashMap<u64, MVal>,
}

// Struct used to configure the Parser. If `None` is passed to any of the fields, then the default
// values are set.
pub struct ParserConfig<'a> {
	arch:         Option<String>,
	default_size: Option<WidthSpec>,
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

		Parser {
			stack:        Vec::new(),
			insts:        Vec::new(),
			opset:        init_opset(),
			opset2:       map_esil_to_opset_2(),
			regset:       regset,
			default_size: default_size,
			arch:         arch,
			tmp_prefix:   tmp_prefix,
			addr:         0,
			opinfo:       None,
			alias_info:   alias_info,
			flags:        flags,
			tmp_index:    0,
			constants:    HashMap::new(),
		}
	}

	pub fn set_register_profile(&mut self, reg_info: &LRegInfo) {//, ssa: &'a mut SSAStorage) {
		// TODO: use SSA methods instead of SSAStorage methods
		// self.ssac = Some(SSAConstruction::new(ssa, reg_info));

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

	fn load_operands(&mut self, signature: Arity) -> Result<(MVal, MVal, WidthSpec), ParseError> {
		match signature {
			Arity::Unary => {
				let op = try!(self.get_param());
				let width = op.size;
				assert!(op.val_type != MValType::Null);
				Ok((op, MVal::null(), width))
			},
			Arity::Binary | Arity::BinaryBool => {
				let mut op1 = try!(self.get_param());
				let mut op2 = try!(self.get_param());
				assert!(op1.val_type != MValType::Null);
				assert!(op2.val_type != MValType::Null);
				let dst_size = cmp::max(op1.size, op2.size);
				self.add_widen_inst(&mut op2, dst_size);
				self.add_widen_inst(&mut op1, dst_size);
				Ok((op1, op2,  if signature == Arity::Binary { dst_size } else { 1 }))
			},
			Arity::BinaryAsym => {
				let op1 = try!(self.get_param());
				let op2 = try!(self.get_param());
				assert!(op1.val_type != MValType::Null);
				assert!(op2.val_type != MValType::Null);
				Ok((op1, op2, 0))
			},
		}
	}

	pub fn add_d_inst(&mut self, op: MOpcode, dst: MVal, op1: MVal, op2: MVal, update_flags: bool) {
		let addr = Some(MAddr::new(self.addr));
		let mut inst = op.to_inst(dst, op1, op2, addr);
		self.insts.push(inst)
	}

	pub fn parse_str(&mut self, esil: &str) -> Result<(), ParseError> {
		let addr = Some(MAddr::new(self.addr));

		if esil.len() == 0 {
			return Err(ParseError::InvalidEsil);
		}

		let esil: Vec<String> = esil.split(',')
			.map(|x| x.to_string())
			.collect();

		for token in esil {

			let description = match self.opset2.get(&*token) {
				Some(description) => description.clone(),
				None => {
					let mut val_type = MValType::Unknown;
					let mut val: Option<u64> = None;
					let mut size: WidthSpec = self.default_size;
					let mut reg_info: Option<MRegInfo> = None;
					if let Some(r) = self.regset.get(&*token) {
						val_type = MValType::Register;
						let mut reg_info_ = self.new_mreg_info(r.clone());
						let alias = self.alias_info.get(&reg_info_.reg)
							.map(|x| x.role_str.clone())
							.unwrap_or_default();
						reg_info_.alias = alias;
						reg_info = Some(reg_info_.clone());
						size = r.size as WidthSpec;

						// return Err(ParseError::InvalidMOperator)
					} /* else if let Ok::<i64, _>(v) = hex_to_i!(token) { // <u64>? will it be able to deal with negative numbers?
						// TODO
						val = Some(v as u64);
					}*/ else if let Ok(num) = token.parse::<i64>() { // <u64>? will it be able to deal with negative numbers?
						val = Some(num as u64);
					} else if let Some('%') = token.chars().nth(0) {
						val_type = MValType::Internal
					} else {
						// TODO
						panic!("Proper error handling here");
					}

					if let Some(cv) = val {
						EsilOperation::PushNumber(cv)
					} else {
						let v = MVal::new(token, size, val_type, 0, reg_info);
						self.stack.push(v);
						continue;
					}
				},
			};

			match description {
				EsilOperation::Plain(op, arity) => {
					let update_flags = op == MOpcode::OpCmp; // TODO: correct this

					let (op1, op2, dst_size) = try!(self.load_operands(arity));
					let dst = self.get_tmp_register(dst_size);
					let inst = self.add_d_inst(op, dst.clone(), op1, op2, update_flags);

					self.stack.push(dst);
				},

				EsilOperation::Assign(op, arity) => {
					let (op1, op2, dst_size) = try!(self.load_operands(arity));
					assert!(op1.val_type == MValType::Register);
					assert!(dst_size == op1.size);
					let mut inst = self.add_d_inst(op, op1.clone(), op1, op2, /*update_flags=*/ true);
				},

				EsilOperation::Memory(op, arity, byte) => {
					let update_flags = op == MOpcode::OpCmp; // TODO: correct this

					let (op1, op2, dst_size) = try!(self.load_operands(arity));

					let op1d = self.get_tmp_register(op2.size);
					let dst = self.get_tmp_register(dst_size);

					self.add_d_inst(MOpcode::OpLoad,  op1d.clone(), op1.clone(),  MVal::null(), false);
					self.add_d_inst(op,               dst.clone(),  op1d.clone(), op2,          update_flags);
					self.add_d_inst(MOpcode::OpStore, MVal::null(), op1.clone(),  dst.clone(),  false);
				},

				EsilOperation::FlowControl(flowop) => match flowop {
					EsilFlowOperation::CSkipOn => {
						//let inst = op.to_inst(MVal::null(), MVal::null(), MVal::null(), addr);
						//self.insts.push(inst);
						unimplemented!();
					},
					EsilFlowOperation::SkipToggle => unimplemented!(),
					EsilFlowOperation::SkipOff    => unimplemented!(),
					EsilFlowOperation::Goto       => unimplemented!(),
					EsilFlowOperation::Break      => unimplemented!(),
				},

				EsilOperation::StackControl(stackop) => match stackop {
					EsilStackOperation::Pop           => {self.stack.pop();},
					EsilStackOperation::Clear         => self.stack.clear(),
					EsilStackOperation::Dup           => unimplemented!(),
					EsilStackOperation::LoadMultiple  => unimplemented!(),
					EsilStackOperation::StoreMultiple => unimplemented!(),
				},

				EsilOperation::PushNumber(n) => {
					let v = self.constant_value(n);
					self.stack.push(v);
				},

				EsilOperation::Interrupt => unimplemented!(),
				EsilOperation::Todo      => unimplemented!(),
				EsilOperation::Trap      => unimplemented!(),
				EsilOperation::Ignore    => {},
			}

			break;
/*

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
			let has_op      = tokens.at(1).unwrap_or("");
			let eq          = tokens.at(2).unwrap_or("");
			let access_size = tokens.at(3).unwrap_or("");
			let access_size = match access_size {
				"" => self.default_size,
				_  => access_size.parse::<WidthSpec>().unwrap() * 8,
			};

			try!(self.add_inst(MOpcode::OpLoad));
			// Set the correct size.
			let mut x = self.stack.pop().unwrap();
			self.add_narrow_inst(&mut x, access_size);
			let tmp_dst1 = x.clone();
			self.stack.push(x);*/
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
					// TODO
					// if _inst.opcode == MOpcode::OpCl {
					// 	break;
					// }
					if _inst.opcode != MOpcode::OpJmp {
						res.push(_inst.clone());
						continue;
					}

					jmp_inst = Some(
						MOpcode::OpCJmp.to_inst(MVal::null(),
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

	fn get_tmp_register(&mut self, mut size: WidthSpec) -> MVal {
		self.tmp_index += 1;
		if size == 0 {
			size = self.default_size;
		}
		MVal::tmp(self.tmp_index, size)
	}

	// Convert a lower field width to higher field width.
	fn add_widen_inst(&mut self, op: &mut MVal, size: WidthSpec) {
		if op.size >= size {
			return;
		}
		let dst = self.get_tmp_register(size);
		let operator = MOpcode::OpWiden(size);
		let addr = MAddr::new(self.addr);
		let inst = operator.to_inst(dst.clone(), op.clone(), MVal::null(), Some(addr));
		self.insts.push(inst.clone());
		*op = dst;
	}

	// Convert a higher field width to lower field width.
	fn add_narrow_inst(&mut self, op: &mut MVal, size: WidthSpec) {
		if op.size <= size {
			return;
		}
		let dst = self.get_tmp_register(size);
		let operator = MOpcode::OpNarrow(size);
		let addr = MAddr::new(self.addr);
		let inst = operator.to_inst(dst.clone(), op.clone(), MVal::null(), Some(addr));
		self.insts.push(inst.clone());
		*op = dst;
	}

	/*fn add_assign_inst(&mut self, op: MOpcode) -> Result<(), ParseError> {
		let     dst = try!(self.stack.pop().ok_or(ParseError::InsufficientOperands));
		let mut op1 = try!(self.get_param());

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
			let inst = op.to_inst(MVal::null(), op1, MVal::null(), Some(addr));
			self.insts.push(inst.clone());
			return Ok(());
		}

		// If the dst is a register. Update the last_assgn information.
		let update_flags = dst.val_type == MValType::Register;

		if dst.size == op1.size {
			let addr = MAddr::new(self.addr);
			let mut inst = op.to_inst(dst.clone(), op1, MVal::null(), Some(addr));
			inst.update_flags ||= update_flags;
			self.insts.push(inst.clone());
			return Ok(());
		}

		if dst.size > op1.size { // TODO: update flags based on correct value
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
			// TODO
			/*
			MOpcode::OpCl => {
				let null = MVal::null();
				let addr = MAddr::new(self.addr);
				let inst = op.to_inst(null.clone(), null.clone(), null.clone(), Some(addr));
				self.insts.push(inst.clone());
				return Ok(());
			},*/
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

		let mut op2 = try!(self.get_param());
		let mut op1: MVal = if op.is_binary() {
			try!(self.get_param())
		} else {
			MVal::null()
		};

		let dst_size = cmp::max(op1.size, op2.size);

		// Add a check to see if dst, op1 and op2 have the same size.
		// If they do not, cast it. op2 is never 'Null'.
		assert!(op2.val_type != MValType::Null);

		if op.is_binary() {
			// not all binaries want to have their operands at same size. consider OpStore.
			self.add_widen_inst(&mut op2, op1.size);
			self.add_widen_inst(&mut op1, op2.size);
		}

		let mut dst = self.get_tmp_register(dst_size);
		let mut update_flags = false;
		if op == MOpcode::OpCmp {
			update_flags = true;
			dst.size = 1;
		}

		if op == MOpcode::OpGt || op == MOpcode::OpLt || op == MOpcode::OpGteq || op == MOpcode::OpLteq {
			dst.size = 1;
		}

		let addr = MAddr::new(self.addr);
		let mut inst = op.to_inst(dst.clone(), op2, op1, Some(addr));
		inst.update_flags = update_flags;
		self.insts.push(inst);
		self.stack.push(dst);

		Ok(())
	}*/

	fn set_parser_index(&mut self, i: u64) {
		self.tmp_index = i;
	}

	// correspons to r_anal_esil_get_parm
	fn get_param(&mut self) -> Result<MVal, ParseError> {
		let v = self.stack.pop();
		let mv = try!(v.ok_or(ParseError::InsufficientOperands));
		if mv.val_type != MValType::Internal {
			return Ok(mv);
		}

		let bit = if mv.name.len() < 3 {
			"0"
		} else {
			&mv.name[2..]
		};

		// Create a dummy temporary parser and initialize.
		let mut tmp_p = Parser::new(None);
		tmp_p.set_parser_index(self.tmp_index);
		tmp_p.addr = self.addr;
		tmp_p.constants = self.constants.clone();
		match mv.name.chars().nth(1).unwrap_or('\0') {
			'%' => {
				let addr = self.addr;
				return Ok(self.constant_value(addr))
			},
			'z' => {
				let s = "==";
				tmp_p.stack.push(self.constant_value(0));
				tmp_p.stack.push(MVal::esilcur());
				try!(tmp_p.parse_str(s));
			},
			'b' => {
				tmp_p.stack.push(MVal::esilold());
				let s = format!("1,{},0x3f,&,0x3f,+,0x3f,&,2,<<,-", bit);
				try!(tmp_p.parse_str(&*s));
				let tmp_mval = tmp_p.stack.last().unwrap().clone();
				try!(tmp_p.parse_str("&"));
				tmp_p.stack.push(tmp_mval);
				tmp_p.stack.push(MVal::esilcur());
				let s = "&,<";
				try!(tmp_p.parse_str(s));
			},
			'c' => {
				tmp_p.stack.push(MVal::esilcur());
				let s = format!("1,{},0x3f,&,2,<<,-", bit);
				try!(tmp_p.parse_str(&*s));
				let tmp_mval = tmp_p.stack.last().unwrap().clone();
				try!(tmp_p.parse_str("&"));
				tmp_p.stack.push(tmp_mval);
				tmp_p.stack.push(MVal::esilold());
				let s = "&,<";
				try!(tmp_p.parse_str(s));
			},
			'p' => {
				let mut s = "1,&,^".to_string();
				tmp_p.stack.push(self.constant_value(1));
				tmp_p.stack.push(MVal::esilcur());
				try!(tmp_p.parse_str(&*s));
				for i in (1..8) {
					tmp_p.stack.push(self.constant_value(1));
					tmp_p.stack.push(self.constant_value(i));
					tmp_p.stack.push(MVal::esilcur());
					s = format!(">>,&,^");
					try!(tmp_p.parse_str(&*s));
				}
			},
			'r' => {
				let def = self.default_size as u64;
				return Ok(self.constant_value(def));
			},
			'o' => {
				//(esil_internal_carry_check (esil, esil->lastsz-1) ^ 
				// esil_internal_carry_check (esil, esil->lastsz-2));
				for i in (1..3) {
					tmp_p.stack.push(MVal::esilcur());
					tmp_p.stack.push(self.constant_value(1));
					tmp_p.stack.push(self.constant_value(i));
					tmp_p.stack.push(MVal::esillastsz());
					let s = format!("-,0x3f,&,2,<<,-");
					try!(tmp_p.parse_str(&*s));
					let tmp_mval = tmp_p.stack.last().unwrap().clone();
					try!(tmp_p.parse_str("&"));
					tmp_p.stack.push(tmp_mval);
					tmp_p.stack.push(MVal::esilold());
					let s = "&,<";
					try!(tmp_p.parse_str(s));
				}
				let s = "^";
				try!(tmp_p.parse_str(s));
			},
			's' => {
				// !!((esil->cur & (0x1<<(esil->lastsz-1)))>>(esil->lastsz-1));
				tmp_p.stack.push(MVal::esilcur());
				tmp_p.stack.push(self.constant_value(1));
				tmp_p.stack.push(MVal::esillastsz());
				let s = "-".to_string();
				try!(tmp_p.parse_str(&*s));
				let v = tmp_p.stack.last().unwrap().clone();
				let s = "1,<<,&";
				try!(tmp_p.parse_str(s));
				tmp_p.stack.insert(0, v.clone());
				let s = ">>";
				try!(tmp_p.parse_str(s));
				let s = "0,==,!";
				try!(tmp_p.parse_str(s));
			},
			_ => panic!("Invalid internal variable!"),
		}

		// Copy generated insts.
		let insts = tmp_p.emit_insts();
		for inst in insts.iter() {
			self.insts.push(inst.clone());
		}

		// Update current parser tmp index.
		self.set_parser_index(tmp_p.tmp_index);
		self.constants = tmp_p.constants.clone();
		let res = insts.last().unwrap().dst.clone();
		Ok(res)
	}

	fn constant_value(&mut self, num: u64) -> MVal {
		let tmp: MVal = if !self.constants.contains_key(&num) {
			let op = MOpcode::OpConst(num);
			let size = self.default_size;
			let mut next_tmp = self.get_tmp_register(size);
			next_tmp.as_literal = Some(num);
			let inst = op.to_inst(next_tmp.clone(), MVal::null(), MVal::null(), None);
			self.insts.push(inst);
			next_tmp
		} else {
			MVal::null()
		};

		let const_v = self.constants.entry(num)
			.or_insert(tmp)
			.clone();

		// Assert that we actually have a constant.
		assert!(const_v.as_literal != None);
		return const_v;
	}
}

fn map_esil_to_opset() -> HashMap<&'static str, MOpcode> {
	// Make a map from esil string to struct MOperator.
	// (operator: &str, op: MOperator).
	// Possible Optimization:  Move to compile-time generation ?
	hash![]
}


#[derive(Clone)]
enum EsilFlowOperation {
	CSkipOn,
	SkipToggle,
	SkipOff,
	Goto,
	Break
}

#[derive(Clone)]
enum EsilStackOperation {
	Pop,
	Clear,
	Dup,
	LoadMultiple,
	StoreMultiple,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Arity {
	Unary,
	Binary,
	BinaryBool,
	BinaryAsym,
}

#[derive(Clone)]
enum EsilOperation {
	Plain(MOpcode, Arity),
	Assign(MOpcode, Arity),
	Memory(MOpcode, Arity, u8),
	FlowControl(EsilFlowOperation),
	StackControl(EsilStackOperation),
	PushNumber(u64),
	Interrupt,
	Todo,
	Trap,
	Ignore,
}

fn add_variants(
	map: &mut HashMap<String, EsilOperation>,
	base_str: &str,
	opc: MOpcode,
	arity: Arity,
	mem_variants: bool)
{
	let base = base_str.to_owned();
	map.insert(base.clone(),        EsilOperation::Plain(opc, arity));
	map.insert(base.clone()+"=",    EsilOperation::Assign(opc, arity));

	if mem_variants { add_mem_variants(map, &(base+"="), opc, arity); }
}

fn add_mem_variants(
	map: &mut HashMap<String, EsilOperation>,
	base_str: &str,
	opc: MOpcode,
	arity: Arity)
{
	let base = base_str.to_owned();
	map.insert(base.clone()+"[]",  EsilOperation::Memory(opc, arity, 0));
	map.insert(base.clone()+"[1]", EsilOperation::Memory(opc, arity, 1));
	map.insert(base.clone()+"[2]", EsilOperation::Memory(opc, arity, 2));
	map.insert(base.clone()+"[4]", EsilOperation::Memory(opc, arity, 4));
	map.insert(base.clone()+"[8]", EsilOperation::Memory(opc, arity, 8));
}

fn map_esil_to_opset_2() -> HashMap<String, EsilOperation> {
	// Make a map from esil string to struct EsilOperator.

	let mut map: HashMap<String, EsilOperation> = HashMap::new();

	map.insert("$".to_owned(),  EsilOperation::Trap);
	map.insert("$$".to_owned(), EsilOperation::Interrupt);
	map.insert("==".to_owned(), EsilOperation::Plain(MOpcode::OpCmp,  Arity::BinaryBool));
	map.insert("<".to_owned(),  EsilOperation::Plain(MOpcode::OpLt,   Arity::BinaryBool));
	map.insert(">".to_owned(),  EsilOperation::Plain(MOpcode::OpGt,   Arity::BinaryBool));
	map.insert("<=".to_owned(), EsilOperation::Plain(MOpcode::OpLteq, Arity::BinaryBool));
	map.insert(">=".to_owned(), EsilOperation::Plain(MOpcode::OpGteq, Arity::BinaryBool));

	// TODO!
	// map.insert("=".to_owned(),  EsilOperation::Assign(MOpcode::OpMov, Arity::VoidBinary));
	// add_mem_variants(&mut map, "=", MOpcode::OpMov, Arity::VoidBinary);
	// add_mem_variants(&mut map, "",  MOpcode::OpLoad, Arity::Unary);

	add_variants(&mut map, "<<", MOpcode::OpLsl, Arity::BinaryAsym, false);
	add_variants(&mut map, ">>", MOpcode::OpLsr, Arity::BinaryAsym, false);
	add_variants(&mut map, "&",  MOpcode::OpAnd, Arity::Binary, true);
	add_variants(&mut map, "|",  MOpcode::OpOr,  Arity::Binary, true);
	add_variants(&mut map, "*",  MOpcode::OpMul, Arity::Binary, true);
	add_variants(&mut map, "^",  MOpcode::OpXor, Arity::Binary, true);
	add_variants(&mut map, "+",  MOpcode::OpAdd, Arity::Binary, true);
	add_variants(&mut map, "-",  MOpcode::OpSub, Arity::Binary, true);
	add_variants(&mut map, "/",  MOpcode::OpDiv, Arity::Binary, true);
	add_variants(&mut map, "%",  MOpcode::OpMod, Arity::Binary, true);
	add_variants(&mut map, "!",  MOpcode::OpNot, Arity::Unary, false);
	add_variants(&mut map, "--", MOpcode::OpDec, Arity::Unary, true);
	add_variants(&mut map, "++", MOpcode::OpInc, Arity::Unary, true);

	map.insert("?{".to_owned(),    EsilOperation::FlowControl(EsilFlowOperation::CSkipOn));
	map.insert("}{".to_owned(),    EsilOperation::FlowControl(EsilFlowOperation::SkipToggle));
	map.insert("}".to_owned(),     EsilOperation::FlowControl(EsilFlowOperation::SkipOff));
	map.insert("GOTO".to_owned(),  EsilOperation::FlowControl(EsilFlowOperation::Goto));
	map.insert("BREAK".to_owned(), EsilOperation::FlowControl(EsilFlowOperation::Break));
	map.insert("STACK".to_owned(), EsilOperation::Ignore);
	map.insert("TODO".to_owned(),  EsilOperation::Todo);
	map.insert("POP".to_owned(),   EsilOperation::StackControl(EsilStackOperation::Pop));
	map.insert("CLEAR".to_owned(), EsilOperation::StackControl(EsilStackOperation::Clear));
	map.insert("DUP".to_owned(),   EsilOperation::StackControl(EsilStackOperation::Dup));
	map.insert("[*]".to_owned(),   EsilOperation::StackControl(EsilStackOperation::LoadMultiple));
	map.insert("=[*]".to_owned(),  EsilOperation::StackControl(EsilStackOperation::StoreMultiple));

	return map
}

