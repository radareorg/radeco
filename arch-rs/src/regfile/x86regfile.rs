use r2api::structs::LRegInfo;
use std::collections::HashMap;
use std::cmp::Ordering;

use crate::utils::*;
use crate::regfile::regfile::*;

/// Basic X86Register structure
#[derive(Clone, Debug, Hash)]
pub struct X86Register {
    name: String,
    regtype: RegType,
    offset: usize,
    width: usize,
    is_whole: bool,
    parent: Option<AbstractRegister>,
}

impl X86Register {
    pub fn new(name: String, regtype: RegType, offset: usize, width: usize, is_whole: bool, parent: Option<AbstractRegister>) -> X86Register {
        X86Register {
            name: name,
            regtype: regtype,
            offset: offset,
            width: width,
            is_whole: is_whole,
            parent: parent
        }
    }
}

impl BasicRegInfo for X86Register {
    /////////////////
    //// Getters ////
    /////////////////

    fn name(&self) -> Option<&String> {
        Some(&self.name)
    }

    fn regtype(&self) -> Option<RegType> {
        Some(self.regtype)
    }

    fn offset(&self) -> Option<usize> {
        Some(self.offset)
    }

    fn width(&self) -> Option<usize> {
        Some(self.width)
    }

    fn is_whole(&self) -> Option<bool> {
        Some(self.is_whole)
    }

    fn parent(&self) -> Option<AbstractRegister> {
        self.parent
    }

    /////////////////
    //// Setters ////
    /////////////////

    fn set_name(&mut self, name: String) {
        self.name = name;
    }

    fn set_regtype(&mut self, regtype: RegType) {
        self.regtype = regtype;
    }

    fn set_offset(&mut self, offset: usize) {
        self.offset = offset;
    }

    fn set_width(&mut self, width: usize) {
        self.width = width;
    }

    fn set_whole(&mut self, is_whole: bool) {
        self.is_whole = is_whole;
    }

    fn set_parent(&mut self, parent: Option<AbstractRegister>) {
        self.parent = parent;
    }
}

/// Basic X86RegisterFile implementation
#[derive(Clone, Debug)]
pub struct X86RegisterFile {
    absregmap: AbsRegMap, // HashMap<String, AbstractRegister>
    registers: HashMap<AbstractRegister, X86Register>,
    aliases: HashMap<String, String>,
    available_abstract_regs: Vec<AbstractRegister>,
}

impl RegisterFile for X86RegisterFile {
    type Reg = X86Register;

    fn register(&self, abs: AbstractRegister) -> Option<&X86Register> {
        self.registers.get(&abs)
    }

    fn register_by_name(&self, reg_name: String) -> Option<&X86Register> {
        if let Some(abs_reg) = self.absregmap.get(&reg_name) {
            self.register(*abs_reg)
        } else {
            None
        }
    }

    fn add_register(&mut self, abs_reg: Option<AbstractRegister>, reg: &X86Register) -> Result<AbstractRegister, RegFileError> {
        if let Some(abs) = abs_reg {
            let idx = self.available_abstract_regs.iter().position( |&t| t == abs);
            if idx.is_none() {
                let _ = self.available_abstract_regs.remove(idx.unwrap());
                self.absregmap.insert(reg.name.clone(), abs);
                self.registers.insert(abs, reg.clone());
                Ok(abs)
            } else {
                *self.registers.get_mut(&self.available_abstract_regs[idx.unwrap()]).unwrap() = reg.clone();
                Ok(abs)
            }
        } else {
            let abs = self.available_abstract_regs.pop();
            if let Some(abs_reg) = abs {
                self.absregmap.insert(reg.name.clone(), abs_reg);
                self.registers.insert(abs_reg, reg.clone());
                Ok(abs_reg)
             } else {
                Err(RegFileError::OutOfAbstractRegisters)
            }
        }
    }

    fn remove_register(&mut self, abs_reg: AbstractRegister) -> Result<AbstractRegister, RegFileError> {
        if self.registers.contains_key(&abs_reg) {
            let reg = self.registers.remove(&abs_reg).unwrap();
            let reg_name = reg.name().unwrap();
            let _ = self.absregmap.remove(reg_name);
            Ok(abs_reg)
        } else {
            Err(RegFileError::AbstractRegisterUnallocated)
        }
    }

    fn absregmap(&self) -> Option<AbsRegMap> {
        Some(self.absregmap.clone())
    }
}

impl X86RegisterFile {
    // Helper method to create new RegisterFile based on information passed
    pub fn new(reg_info: &LRegInfo) -> Result<X86RegisterFile, RegFileError> {
        let mut absregmap: AbsRegMap = HashMap::new();
        let mut available_abstract_regs = abstract_values();
        let mut regmap = HashMap::new();
        let mut aliases = HashMap::new();
        let mut registers = Vec::new();

        for reg in &reg_info.alias_info {
            aliases.insert(reg.reg.clone(), reg.role_str.clone());
        }

        for reg in reg_info.reg_info.iter() {
            let regtype = RegType::from(reg.regtype);
            match regtype {
                RegType::FloatingPoint | RegType::Flag => continue,
                // name regtype offset width parent
                _ => registers.push(X86Register::new(reg.name.clone(), regtype, reg.offset, reg.size, false, None)),
            }
        }

        registers.sort_by(|a, b| {
            let ord = a.offset.cmp(&b.offset);
            if let Ordering::Equal = ord {
                (b.offset + b.width).cmp(&(a.offset + a.width))
            } else {
                ord
            }
        });

        let mut c_reg;
        let mut parent_reg = X86Register::new(String::from("TEMP"), RegType::Unknown, 0, 0, false, None);

        let mut abs;
        let mut parent_abs = AbstractRegister::WILDCARD;

        let mut c_until;

        for reg in &registers {
            c_reg = reg.clone();
            c_until = parent_reg.offset + parent_reg.width;

            abs = available_abstract_regs.pop();
            if abs.is_none() {
                return Err(RegFileError::OutOfAbstractRegisters)
            }

            if reg.offset >= c_until {
                parent_reg = reg.clone();
                parent_abs = abs.unwrap();
                c_reg.set_whole(true);
                c_reg.set_parent(None);
            } else {
                c_reg.set_whole(false);
                c_reg.set_parent(Some(parent_abs));
                assert!((reg.width + reg.offset) <= c_until)
            }

            c_reg.set_offset(reg.offset - parent_reg.offset);

            absregmap.insert(reg.name.clone(), abs.unwrap());
            regmap.insert(abs.unwrap(), c_reg);

        }

        Ok(X86RegisterFile {
            absregmap: absregmap,
            registers: regmap,
            aliases: aliases,
            available_abstract_regs: available_abstract_regs,
        })
    }
}
