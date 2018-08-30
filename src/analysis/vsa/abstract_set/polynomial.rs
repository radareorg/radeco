// Copyright (c) 2018, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements math operation on the multivariate polynomial.
//!
//! A polynomial abstract set goes like:
//!     base + [a1]x1 + [a2]x2 + ... + [an]xn
//! which means:
//!     {base + k1 * x1 + k2 * x2 + ... + kn * xn | 0 <= ki <= ai}

// TODO
