// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Holds traits and structs for the ssa form

// TODO: 
//       - Decide the structs and traits to expose through ssa:: namespace,
//         i.e. Decide what we want to re export for convienence.
//       - Decide which members need to be private and never exposed.

pub mod cfg_traits;
pub mod ssa_traits;
pub mod ssastorage;
pub mod ssadot;
pub mod verifier;
pub mod error;

pub use self::ssa_traits::{SSA, SSAMod, NodeData, ValueType, BBInfo};
pub use self::ssastorage::{SSAStorage};

// TODO: Fix/Complete SSAQuote before re enabling.
//pub mod ssaquote;
//pub use self::ssaquote::*;
