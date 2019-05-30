// Copyright (c) 2018, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements value set analysis on radeco IR.
//! More information are available from Gogul Balakrishnan's Ph.D. Thesis.
//! Please refer here:
//!     * https://research.cs.wisc.edu/wpis/papers/balakrishnan_thesis.pdf

pub mod abstract_set {
    pub mod abstract_set;
    pub mod bdd;
    pub mod polynomial;
    pub mod strided_interval;
}
