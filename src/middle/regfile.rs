use std::cmp::Ordering;
use std::collections::HashMap;
use frontend::structs::LRegInfo;
use middle::ssa::{BBInfo, SSAMod, ValueType};
use middle::ir::{MOpcode, WidthSpec};
use transform::phiplacement::PhiPlacer;

pub struct SubRegister {
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
				width: current.2 - current.1
			});
		}
		SubRegisterFile {
			whole_registers: whole,
			named_registers: slices
		}
	}

	pub fn write_register<'a, T: SSAMod<BBInfo=BBInfo> + 'a>(
		&self, phiplacer: &mut PhiPlacer<'a, T>, base: usize,
		block: T::ActionRef,
		var: &String,
		value: T::ValueRef
	) {
		let info = &self.named_registers[var];
		let id = info.base + base;
		// TODO
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
		match phiplacer.variable_types[id] {
			ValueType::Integer{width} => {
				let mut value = phiplacer.read_variable(block, id);
				if info.shift > 0 {
					let shift_amount_node = phiplacer.ssa.add_const(block, info.shift as u64);
					let new_value = phiplacer.ssa.add_op(block, MOpcode::OpLsr, ValueType::Integer{width: info.width as WidthSpec});
					phiplacer.ssa.op_use(new_value, 0, value);
					phiplacer.ssa.op_use(new_value, 1, shift_amount_node);
					value = new_value;
				}
				if (width as usize) < info.width {
					let new_value = phiplacer.ssa.add_op(block, MOpcode::OpNarrow(width), ValueType::Integer{width: width as WidthSpec});
					phiplacer.ssa.op_use(new_value, 0, value);
					value = new_value;
				}
				value
			},
			/*_ => unimplemented!()*/
		}
	}
}
