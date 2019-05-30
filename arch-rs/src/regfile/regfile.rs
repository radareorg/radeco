//! Defines traits which describe a RegisterFile
use r2api::structs::LRegInfo;
use std::collections::HashMap;
use utils::*;

/// Register Type
#[derive(Clone, Debug, Copy, Hash)]
pub enum RegType {
    GeneralPurpose,
    Debug,
    FloatingPoint,
    XMM,
    Flag,
    Segment,
    Unknown
}

/// Helper method to convert RegisterType returned by radare2 to RegType enum
// Could be a part of radare2-r2pipe-api later.
impl From<usize> for RegType {
    fn from(regtype: usize) -> RegType {
        match regtype {
            0 => RegType::GeneralPurpose,
            1 => RegType::Debug,
            2 => RegType::FloatingPoint,
            3 =>  RegType::XMM,
            4 => RegType::Unknown,
            5 => RegType::Flag,
            6 => RegType::Segment,
            _ => RegType::Unknown
        }
    }
}

/// Error type for RegisterFile
#[derive(Clone, Copy, Debug)]
pub enum RegFileError {
    AbstractRegisterAlreadyAllocated,
    AbstractRegisterUnallocated,
    OutOfAbstractRegisters,
    RegisterNotFound
}

/// Trait defining methods to be implemented by a Register
/// This can be hence used in different projects if the users want to describe their own register
/// struct.
pub trait BasicRegInfo {
    /////////////////
    //// Getters ////
    /////////////////

    /// Get name of the register
    fn name(&self) -> Option<&String>;

    /// Get register type
    fn regtype(&self) -> Option<RegType>;

    /// Get register width
    fn width(&self) -> Option<usize>;

    /// Get register offset in the parent
    fn offset(&self) -> Option<usize>;

    /// Check if the register is full
    fn is_whole(&self) -> Option<bool>;

    /// Get the parent of the register
    fn parent(&self) -> Option<AbstractRegister>;

    /////////////////
    //// Setters ////
    /////////////////
    
    /// Set name of the register
    fn set_name(&mut self, String);

    /// Set register type
    fn set_regtype(&mut self, RegType);

    /// Set register offset
    fn set_offset(&mut self, usize);

    /// Set register width
    fn set_width(&mut self, usize);

    /// Set register wholeness
    fn set_whole(&mut self, bool);

    /// Set the parent of the register
    fn set_parent(&mut self, Option<AbstractRegister>);
}

/// Trait defining methods which should be implemented by a struct which aims to be a RegisterFile
pub trait RegisterFile {
    type Reg: BasicRegInfo + Clone;

    /// Get register information by AbstractRegister-based indexing
    fn register(&self, AbstractRegister) -> Option<&Self::Reg>;

    /// Get register information using name of the register
    fn register_by_name(&self, String) -> Option<&Self::Reg>;

    /// Insert information for a new register to the RegisterFile. 
    /// The next available AR is provided.
    fn add_register(&mut self, Option<AbstractRegister>, &Self::Reg) -> Result<AbstractRegister, RegFileError>;

    /// Remove register allocation for an AbstractRegister
    fn remove_register(&mut self, AbstractRegister) -> Result<AbstractRegister, RegFileError>;

    /// Get associated register mapping for the RegisterFile.
    fn absregmap(&self) -> Option<AbsRegMap>;
}

/// Create a new absregmap from passed register information
pub fn new_absregmap(reg_info: &LRegInfo) -> Result<AbsRegMap, RegFileError> {
    let mut available_abstract_regs = abstract_values();
    let mut absregmap = HashMap::new();

    for register in &reg_info.reg_info {
       if let Some(abs) = available_abstract_regs.pop() {
           absregmap.insert(register.name.clone(), abs);
       } else {
           return Err(RegFileError::OutOfAbstractRegisters)
       }
    }

    Ok(absregmap)
}
