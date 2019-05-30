// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

/// Design decisions for Parser and VM.
///
/// Idea1:
/// ```
///		evaluator = Evaluator::new();
///		p = Parser::init(&evaluator);
///		p.parse("eax, ebx, +=");
///		p.parse("ecx, eax, =");
///		p.results();
/// ```
///
/// Idea2:
/// ```
///		p = Parser::new();
///		evaluator = Evaluator::init(&p);
///		evaluator.run()
///     
///     impl Evaluator {
///			fn run() {
///				self.p.parse(&self, insts);
///				// "a,b,+=,$z,zf,="
///			}
///		}
///	```
///
///
///
///
///	 -----------
///  | Evaluator|  <-> | Parser |
///	 ____________
