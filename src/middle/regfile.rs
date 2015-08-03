use std::cmp::Ordering;
use std::collections::HashMap;
use frontend::structs::LRegInfo;
use middle::ssa::{BBInfo, SSAMod, ValueType};
use middle::ir::{MOpcode, WidthSpec};
use middle::phiplacement::PhiPlacer;

#[derive(Debug)]
pub struct SubRegister {
	// usize or WidthSpec
	pub base:  usize,
	pub shift: usize,
	pub width: usize
}

pub struct SubRegisterFile {
	pub whole_registers: Vec<ValueType>, // methods don't use this, ownership transfer of whole_r outwards desirable (currently cloning)
	pub named_registers: HashMap<String, SubRegister>
}

impl SubRegisterFile {
	pub fn new(reg_info: &LRegInfo) -> SubRegisterFile {
		let mut slices = HashMap::new();
		let mut events: Vec<(usize, usize, usize)> = Vec::new();
		for (i, reg) in reg_info.reg_info.iter().enumerate() {
			// println!("{:?}", reg);
			if reg.type_str != "gpr" { continue; } // st7 from "fpu" overlaps with zf from "gpr" (r2 bug?)
			if reg.name.ends_with("flags") { continue; } // HARDCODED x86
			events.push((i, reg.offset, reg.offset + reg.size));
		}

		events.sort_by(|a, b| {
			let o = a.1.cmp(&b.1);
			match o {
				Ordering::Equal => b.2.cmp(&a.2),
				_ => o
			}
		});

		let mut current: (usize, usize, usize) = (0, 0, 0);
		let mut whole: Vec<ValueType> = Vec::new();
		for &ev in &events {
			if ev.1 >= current.2 {
				current = ev;
				whole.push(ValueType::Integer { width: (current.2 - current.1) as WidthSpec });
			} else {
				assert!(ev.2 <= current.2);
			}
			slices.insert(reg_info.reg_info[ev.0].name.clone(), SubRegister {
				base: whole.len()-1,
				shift: ev.1 - current.1,
				width: ev.2 - ev.1
			});
		}
		/*for (ref name, ref sr) in &slices {
		  println!("{} = (u{})(r{}>>{})", name, sr.width, sr.base, sr.shift);
		  }*/
		SubRegisterFile {
			whole_registers: whole,
			named_registers: slices
		}
	}

	pub fn write_register<'a, T: SSAMod<BBInfo=BBInfo> + 'a>(
		&self, phiplacer: &mut PhiPlacer<'a, T>, base: usize,
		block: T::ActionRef,
		var: &String,
		mut value: T::ValueRef
	) {
		let info = &self.named_registers[var];
		let id = info.base + base;
		match phiplacer.variable_types[id] {
			ValueType::Integer{width} if info.width < (width as usize) => {
				//println!("Assigning to {:?}: {:?}", var, info);
				let vt = ValueType::Integer{width: width as WidthSpec};

				let mut new_value = phiplacer.ssa.add_op(block, MOpcode::OpWiden(width as WidthSpec), vt);
				phiplacer.ssa.op_use(new_value, 0, value);
				value = new_value;

				if info.shift > 0 {
					//println!("Shifting by {:?}", info.shift);
					let shift_amount_node = phiplacer.ssa.add_const(block, info.shift as u64);
					new_value = phiplacer.ssa.add_op(block, MOpcode::OpLsl, vt);
					phiplacer.ssa.op_use(new_value, 0, value);
					phiplacer.ssa.op_use(new_value, 1, shift_amount_node);
					value = new_value;
				}

				let fullvalue: u64 = !((!1u64) << (width-1));
				let maskvalue: u64 = ((!((!1u64) << (info.width-1))) << info.shift) ^ fullvalue;

				if maskvalue != 0 {
					//println!("Masking with {:?}", maskvalue);
					let mut ov = phiplacer.read_variable(block, id);
					let maskvalue_node = phiplacer.ssa.add_const(block, maskvalue);

					new_value = phiplacer.ssa.add_op(block, MOpcode::OpAnd, vt);
					phiplacer.ssa.op_use(new_value, 0, ov);
					phiplacer.ssa.op_use(new_value, 1, maskvalue_node);
					ov = new_value;

					new_value = phiplacer.ssa.add_op(block, MOpcode::OpOr, vt);
					phiplacer.ssa.op_use(new_value, 0, value);
					phiplacer.ssa.op_use(new_value, 1, ov);
					value = new_value;
				}
			},
			ValueType::Integer{..} => (),
			_ => unimplemented!()
		}
		phiplacer.write_variable(block, id, value);
	}

	pub fn read_register<'a, T: SSAMod<BBInfo=BBInfo> + 'a>(
		&self, phiplacer: &mut PhiPlacer<'a, T>,
		base: usize,
		block: T::ActionRef,
		var: &String
		) ->
		T::ValueRef
	{
		let info = &self.named_registers[var];
		let id = info.base + base;
		let mut value = phiplacer.read_variable(block, id);
		match phiplacer.variable_types[id] {
			ValueType::Integer{width} => {
				if info.shift > 0 {
					let shift_amount_node = phiplacer.ssa.add_const(block, info.shift as u64);
					let new_value = phiplacer.ssa.add_op(block, MOpcode::OpLsr, ValueType::Integer{width: width as WidthSpec});
					phiplacer.ssa.op_use(new_value, 0, value);
					phiplacer.ssa.op_use(new_value, 1, shift_amount_node);
					value = new_value;
				}
				if (width as usize) < info.width {
					let new_value = phiplacer.ssa.add_op(block, MOpcode::OpNarrow(info.width as WidthSpec), ValueType::Integer{width: info.width as WidthSpec});
					phiplacer.ssa.op_use(new_value, 0, value);
					value = new_value;
				}
				value
			},
			_ => unimplemented!()
		}
	}
}
