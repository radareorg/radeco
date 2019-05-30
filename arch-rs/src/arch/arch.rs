//! Defines `Architecture` trait.

use r2api::structs::Endian;

use cc::calling_convention::*;
use regfile::regfile::*;

/// Defines a generic trait which all architectures have to implement,
/// over and above their own specifics
pub trait Architecture {
    /// Calling Convention to be used for this Architecture
    type CC: CallingConvention + Clone;
    type RF: RegisterFile + Clone;

    /////////////////
    //// Getters ////
    /////////////////

    /// Get architecture name
    fn name(&self) -> Option<&String>;

    /// Get memory Endianness of the architecture.
    /// Option is Endian::Big or Endian::Little
    fn endianness(&self) -> Option<Endian>;

    /// Get word length for instructions in number of bits
    fn word_length(&self) -> Option<u32>;

    /// Get size of an integer variable on the processor architecture
    fn int_size(&self) -> Option<u32>;

    /// Get size of long variable on the processor architectire
    fn long_size(&self) -> Option<u32>;

    /// Return instance of calling convention used for the implementation
    fn calling_convention(&self) -> Option<Self::CC>;

    /// Return instance of register file used for this implementation
    fn register_file(&self) -> Option<Self::RF>;

    /////////////////
    //// Setters ////
    /////////////////
 
    /// Get architecture name
    fn set_name(&mut self, String);

    /// Set memory Endianness of the architecture
    fn set_endianness(&mut self, Endian);

    /// Get word length for instructions in number of bits
    fn set_word_length(&mut self, u32);

    /// Get size of an integer variable on the processor architecture
    fn set_int_size(&mut self, u32);

    /// Get size of long variable on the processor architectire
    fn set_long_size(&mut self, u32);

    /// Return instance of calling convention used for the implementation
    fn set_calling_convention(&mut self, Self::CC);

    /// Return instance of register file used for this implementation
    fn set_register_file(&mut self, Self::RF);

    /* TODO: Possible additions(as taken from various sources):
     * Function prologues
     * Function epilogues
     * Ret instruction
     * Nop instruction
     */
}
