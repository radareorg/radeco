// TODO: 
//       - Decide the structs and traits to expose through ssa:: namespace,
//         i.e. Decide what we want to re export for convienence.
//       - Decide which members need to be private and never exposed.

pub mod ssa_traits;
pub mod ssastorage;
pub mod ssadot;
pub mod phiplacement;
pub mod regfile;

pub use self::ssa_traits::{SSA, SSAMod, NodeData, ValueType, BBInfo};
pub use self::ssastorage::{SSAStorage};
pub use self::phiplacement::{PhiPlacer};
pub use self::regfile::{SubRegisterFile};


// TODO: Fix/Complete SSAQuote before re enabling.
//pub mod ssaquote;
//pub use self::ssaquote::*;
