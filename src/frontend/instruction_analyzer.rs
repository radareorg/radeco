//! Implements Instruction-level analysis used for lifting when information from radare2 is
//! insufficient.
//!
//! Instruction analysis may be abbreviated as `IA`
use std::borrow::Cow;

use capstone_rust::capstone as cs;

// TODO: Register must be replaced by Register information from arch-rs.
// This will be a part of a bigger rewrite/refactor.
/// Describes operands for an instruction
#[derive(Debug, Clone)]
pub enum IOperand {
    Register(String),
    Constant(i64),
    Immediate(i64),
    Memory {
        base: Option<String>,
        index: Option<String>,
        scale: i32,
        disp: i64,
    },
    Other,
    Invalid,
}

/// Holds analysis information for a single instruction
#[derive(Debug, Clone, Default)]
pub struct InstructionInfo {
    mnemonic: Cow<'static, str>,
    reads: Vec<IOperand>,
    writes: Vec<IOperand>,
}

/// Errors from Instruction-level Analysis
pub type IAError = Cow<'static, str>;

/// Trait for a struct to be an Instruction-level analyzer
pub trait InstructionAnalyzer: Sized {
    /// Construct the IA
    fn new(bytes: Vec<u8>) -> Result<Self, IAError>;

    /// Return analyzed information
    fn info(&self) -> Result<&InstructionInfo, IAError>;

    fn mnemonic(&self) -> &Cow<'static, str> {
        &self.info().expect("Unable to get InstructionInfo").mnemonic
    }
    
    /// Return a `Vec` of registers that are read by this instruction
    fn registers_read(&self) -> Vec<&IOperand> {
        self.info().expect("Unable to get InstructionInfo").reads.iter().filter(|&x| match x {
            &IOperand::Register(_) => true,
            _ => false,
        }).collect()
    }

    /// Return a `Vec` of registers that are written to by this instruction
    fn registers_written(&self) -> Vec<&IOperand> {
        self.info().expect("Unable to get InstructionInfo").writes.iter().filter(|&x| match x {
            &IOperand::Register(_) => true,
            _ => false,
        }).collect()
    }

    fn has_memory_operand(&self) -> bool {
        self.has_memory_read() || self.has_memory_written()
    }

    /// Return if this instruction reads from memory
    fn has_memory_read(&self) -> bool {
        self.info().expect("Unable to get InstructionInfo").reads.iter().any(|x| match x { 
            &IOperand::Memory { base: _, index: _, scale: _, disp: _ } => true,
            _ => false,
        })
    }

    /// Return if this instruction writes to memory
    fn has_memory_written(&self) -> bool {
        self.info().expect("Unable to get InstructionInfo").writes.iter().any(|x| match x { 
            &IOperand::Memory { base: _, index: _, scale: _, disp: _ } => true,
            _ => false,
        })
    }

    /// Returns memory written to by the instruction, in standard sib form
    fn memory_written(&self) -> Option<&IOperand> {
        self.info().expect("Unable to get InstructionInfo").writes.iter().find(|&x| match x {
            &IOperand::Memory { base: _, index: _, scale: _, disp: _ } => true,
            _ => false,
        })
    }

    /// Returns memory read by the instruction, in standard sib form
    fn memory_read(&self) -> Option<&IOperand> {
        self.info().expect("Unable to get InstructionInfo").reads.iter().find(|&x| match x {
            &IOperand::Memory { base: _, index: _, scale: _, disp: _ } => true,
            _ => false,
        })
    }
}

/// Capstone-based x86 instruction analyzer
pub struct X86_CS_IA {
    bytes: Vec<u8>,
    cs: cs::Capstone,
    info: InstructionInfo,
}

//impl From<cs::CsErr> for IAError {
    //fn from(err: cs::CsErr) -> IAError {
        //Cow::from(err.to_string())
    //}
//}

impl X86_CS_IA {

    fn reg_name(&self, reg: u32) -> Option<String> {
        self.cs.reg_name(reg).map(|x| x.to_owned())
    }

    fn analyze(&mut self) -> Result<(), IAError> {
        // Disassembly exactly one instruction
        let buf = match self.cs.disasm(self.bytes.as_slice(), 0x0, 1) {
            Ok(d) => d,
            Err(e) => return Err(Cow::from(e.to_string())),
        };

        for instr in buf.iter() {
            // TODO: Look into prefixes for opcode.
            // See how this information is set in capstone.
            self.info.mnemonic = Cow::from(instr.mnemonic);
            let details = instr.detail.unwrap();
            if let cs::DetailsArch::X86(arch) = details.arch {
                for i in 0..arch.op_count {
                    // Get the current operand
                    let op: cs::cs_x86_op = arch.operands[i as usize];
                    let iop: IOperand = match op.type_ {
                        cs::x86_op_type::X86_OP_REG => {
                            let reg: cs::x86_reg = op.reg();
                            if let Some(reg_name) = self.reg_name(reg.as_int()) {
                                IOperand::Register(reg_name)
                            } else {
                                IOperand::Invalid
                            }
                        },
                        cs::x86_op_type::X86_OP_MEM => {
                            let mem: &cs::x86_op_mem = op.mem();
                            let base = self.reg_name(mem.base);
                            let index = self.reg_name(mem.index);
                            let scale = mem.scale;
                            let disp = mem.disp;
                            IOperand::Memory {
                                base: base,
                                index: index,
                                scale: scale,
                                disp: disp,
                            }
                        },
                        cs::x86_op_type::X86_OP_IMM => IOperand::Immediate(op.imm()),
                        // FP/XMM/etc. We can handle these later. Right now, we only
                        // bother about registers (gpr) and memory operands.
                        _ => IOperand::Other,
                    };

                    if i == 0 {
                        self.info.reads.push(iop);
                    } else {
                        self.info.writes.push(iop);
                    }
                }
            }
        }
        Ok(())
    }
}

impl InstructionAnalyzer for X86_CS_IA {
    fn new(bytes: Vec<u8>) -> Result<X86_CS_IA, IAError> {
        let dis = cs::Capstone::new(cs::cs_arch::CS_ARCH_X86, cs::CS_MODE_32).unwrap();
        dis.option(cs::cs_opt_type::CS_OPT_DETAIL, cs::cs_opt_value::CS_OPT_ON).unwrap();

        let mut ia = X86_CS_IA { bytes: bytes, cs: dis, info: InstructionInfo::default() };

        ia.analyze()?;

        Ok(ia)
    }

    fn info(&self) -> Result<&InstructionInfo, IAError> {
        Ok(&self.info)
    }
}
