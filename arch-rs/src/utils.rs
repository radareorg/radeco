use std::collections::HashMap;

/// Declares a general architecture along with it's identifying information
#[macro_export]
macro_rules! declare_architecture {
    (pub struct $arch:ident { $( $field:ident:$ty:ty ),* })  => {
        #[derive(Clone, Debug)]
        pub struct $arch<CC: CallingConvention, RF: RegisterFile> {
            name: String,
            endian: Endian,
            bits: u32,
            int_size: u32,
            long_size: u32,
            calling_convention: Option<CC>,
            regfile: Option<RF>,
            $($field: $ty),*
        }
    };
}

/// Registers the above architecture with a set of getter functions
#[macro_export]
macro_rules! register_architecture {
    ($arch:ident) => {
        impl<CC, RF> Architecture for $arch<CC, RF> 
        where CC: CallingConvention + Clone,
              RF: RegisterFile + Clone
        {
            type CC = CC;
            type RF = RF;

            fn name(&self) -> Option<&String> {
                Some(&self.name)
            }

            fn endianness(&self) -> Option<Endian> {
                Some(self.endian)
            }

            fn word_length(&self) -> Option<u32> {
                Some(self.bits)
            }

            fn int_size(&self) -> Option<u32> {
                Some(self.int_size)
            }

            fn long_size(&self) -> Option<u32> {
                Some(self.long_size)
            }

            fn calling_convention(&self) -> Option<Self::CC> {
                self.calling_convention.clone()
            }

            fn register_file(&self) -> Option<Self::RF> {
                self.regfile.clone()
            }

            fn set_name(&mut self, name: String) {
                self.name = name;
            }

            fn set_endianness(&mut self, endian: Endian) {
                self.endian = endian;
            }

            fn set_word_length(&mut self, bits: u32) {
                self.bits = bits;
            }

            fn set_int_size(&mut self, int_size: u32) {
                self.int_size = int_size;
            }

            fn set_long_size(&mut self, long_size: u32) {
                self.long_size = long_size;
            }

            fn set_calling_convention(&mut self, cc: Self::CC) {
                self.calling_convention = Some(cc);
            }

            fn set_register_file(&mut self, rf: Self::RF) {
                self.regfile = Some(rf);
            }
        }
    };
}

/// AbsRegMap defines a mapping between a register name string and an AbstractRegister
pub type AbsRegMap = HashMap<String, AbstractRegister>;

/// Declares a calling convention
#[macro_export]
macro_rules! declare_cc {
     (pub struct $cc:ident { $( $field:ident:$ty:ty ),* })  => {
        #[derive(Clone, Debug)]
        pub struct $cc {
            name: String,
            arg_placement: ArgPlacement,
            fp_arg_placement: ArgPlacement,
            stack_sp_diff: u64,
            return_val: VType,
            return_addr: VType,
            callee_cleanup: bool,
            absregmap: Option<AbsRegMap>,
            $($field: $ty),*
        }
    };
}

/// Registers a calling convention
#[macro_export]
macro_rules! register_cc {
    ($cc:ident) => {
        impl CallingConvention for $cc {
            fn name(&self) -> Option<&String> {
                Some(&self.name)
            }

            fn arg_placement(&self) -> Option<ArgPlacement> {
                Some(self.arg_placement)
            }

            fn fp_arg_placement(&self) -> Option<ArgPlacement> {
                Some(self.fp_arg_placement)
            }

            fn stack_sp_diff(&self) -> Option<u64> {
                Some(self.stack_sp_diff)
            }

            fn return_val(&self) -> Option<VType> {
                Some(self.return_val)
            }

            fn return_addr(&self) -> Option<VType> {
                Some(self.return_addr)
            }

            fn callee_cleanup(&self) -> Option<bool> {
                Some(self.callee_cleanup)
            }

            fn absregmap(&self) -> Option<AbsRegMap> {
                self.absregmap.clone()
            }

            fn set_name(&mut self, name: String) {
                self.name = name;
            }

            fn set_arg_placement(&mut self, arg_p: ArgPlacement) {
                self.arg_placement = arg_p;
            }

            fn set_fp_arg_placement(&mut self, fp_arg_p: ArgPlacement) {
                self.fp_arg_placement = fp_arg_p;
            }

            fn set_stack_sp_diff(&mut self, stack_diff: u64) {
                self.stack_sp_diff = stack_diff;
            }

            fn set_return_val(&mut self, v: VType) {
                self.return_val = v;
            }

            fn set_return_addr(&mut self, v: VType) {
                self.return_addr = v;
            }

            fn set_callee_cleanup(&mut self, cc: bool) {
                self.callee_cleanup = cc;
            }
            
            fn set_absregmap(&mut self, absregmap: AbsRegMap) {
                self.absregmap = Some(absregmap);
            }
        }
    }
}

macro_rules! define_abstract_registers {
    ($name:ident { $($variant:ident),* $(,)* }) =>
    {
        #[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
        pub enum $name {
            $($variant),*,
        }

       pub fn abstract_values() -> Vec<$name> {
           [$($name::$variant),*].to_vec()
       }
    }
}

define_abstract_registers!(AbstractRegister {
    WILDCARD,
	AR_R127,
	AR_R126,
	AR_R125,
	AR_R124,
	AR_R123,
	AR_R122,
	AR_R121,
	AR_R120,
	AR_R119,
	AR_R118,
	AR_R117,
	AR_R116,
	AR_R115,
	AR_R114,
	AR_R113,
	AR_R112,
	AR_R111,
	AR_R110,
	AR_R109,
	AR_R108,
	AR_R107,
	AR_R106,
	AR_R105,
	AR_R104,
	AR_R103,
	AR_R102,
	AR_R101,
	AR_R100,
	AR_R99,
	AR_R98,
	AR_R97,
	AR_R96,
	AR_R95,
	AR_R94,
	AR_R93,
	AR_R92,
	AR_R91,
	AR_R90,
	AR_R89,
	AR_R88,
	AR_R87,
	AR_R86,
	AR_R85,
	AR_R84,
	AR_R83,
	AR_R82,
	AR_R81,
	AR_R80,
	AR_R79,
	AR_R78,
	AR_R77,
	AR_R76,
	AR_R75,
	AR_R74,
	AR_R73,
	AR_R72,
	AR_R71,
	AR_R70,
	AR_R69,
	AR_R68,
	AR_R67,
	AR_R66,
	AR_R65,
	AR_R64,
	AR_R63,
	AR_R62,
	AR_R61,
	AR_R60,
	AR_R59,
	AR_R58,
	AR_R57,
	AR_R56,
	AR_R55,
	AR_R54,
	AR_R53,
	AR_R52,
	AR_R51,
	AR_R50,
	AR_R49,
	AR_R48,
	AR_R47,
	AR_R46,
	AR_R45,
	AR_R44,
	AR_R43,
	AR_R42,
	AR_R41,
	AR_R40,
	AR_R39,
	AR_R38,
	AR_R37,
	AR_R36,
	AR_R35,
	AR_R34,
	AR_R33,
	AR_R32,
	AR_R31,
	AR_R30,
	AR_R29,
	AR_R28,
	AR_R27,
	AR_R26,
	AR_R25,
	AR_R24,
	AR_R23,
	AR_R22,
	AR_R21,
	AR_R20,
	AR_R19,
	AR_R18,
	AR_R17,
	AR_R16,
	AR_R15,
	AR_R14,
	AR_R13,
	AR_R12,
	AR_R11,
	AR_R10,
	AR_R9,
	AR_R8,
	AR_R7,
	AR_R6,
	AR_R5,
	AR_R4,
	AR_R3,
	AR_R2,
	AR_R1,
	AR_R0,
});
