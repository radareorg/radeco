//! Implements methods and structs required to go from binary to SSA
//! 
//! Also acts a gateway for users to use the library. Check containers
//! submodule for more information.

pub mod ssaconstructor;

// Old/deprecated
pub mod source;
pub mod containers;
/*********************/

// New replacements
pub mod radeco_source;
pub mod radeco_containers;

pub mod bindings;
pub mod instruction_analyzer;
pub mod llanalyzer;
