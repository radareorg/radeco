//! `PathExplorer` that allows interactive exploration

use rune::explorer::explorer::PathExplorer;
use rune::explorer::interactive::Command;
use rune::context::rune_ctx::RuneContext;
use rune::context::context::{Context, Evaluate, MemoryRead, RegisterRead};
use rune::engine::rune::RuneControl;
use rune::memory::seg_mem::SegMem;
use rune::regstore::regfile::RuneRegFile;

use libsmt::theories::{bitvec, core};
use libsmt::logics::qf_abv::QF_ABV_Fn;
use libsmt::backends::z3;
use crate::console::Console;

use std::process;

#[derive(Debug, Clone, Default)]
pub struct InteractiveExplorer {
    console: Console,
    cmd_q: Vec<Command>,
    single_step: bool,
    // TODO: Remove this breakpointing feature once BPs are implemented.
    pub bp: Vec<u64>,
}

impl InteractiveExplorer {
    // Adds Assertions for safety.
    pub fn safety(&self, ctx: &mut RuneContext<SegMem, RuneRegFile>) {
        let rbp = ctx.reg_read("rbp");
        let const_8 = ctx.define_const(8, 64);
        let ret_addr = ctx.eval(bitvec::OpCodes::BvAdd, vec![rbp, const_8]);
        // Add an assertion to check if that memory address can be junk (0x41414141)
        let const_trash = ctx.define_const(0x41414141, 64);
        let mem_at_addr = ctx.mem_read(ret_addr, 64);
        ctx.eval(core::OpCodes::Cmp, vec![mem_at_addr, const_trash]);
    }

    pub fn print_debug(&self, ctx: &RuneContext<SegMem, RuneRegFile>) {
        self.console.print_info("DEBUG");
        self.console.print_info(&format!("Constraints:\n{}", ctx.solver.generate_asserts()));
    }

    pub fn query_constraints(&self, ctx: &mut RuneContext<SegMem, RuneRegFile>) {
        let mut z3: z3::Z3 = Default::default();
        let result = ctx.solve(&mut z3);

        println!("{:?}", result);
        self.console.print_success("Results:");

        /*
        for (k, v) in &ctx.syms {
            if let Some(res) = result.get(v) {
                self.console.print_success(&format!("{} = {:#x}", k, res))
            }
        }
        */
    }

    pub fn add_assertion(&self, ctx: &mut RuneContext<SegMem, RuneRegFile>) {
        self.console.print_assertion_help();
        if let Ok(ref line) = self.console.readline() {
            // Format for adding assertions:
            // (operation) (register) (register/constant in hex)
            // Valid Operations: =, >, <, <=, >=
            let tokens = line.trim().split(' ').collect::<Vec<&str>>();
            let cmd: QF_ABV_Fn = match tokens[0] {
                ">=" => bitvec::OpCodes::BvUGe.into(),
                "<=" => bitvec::OpCodes::BvULe.into(),
                ">" => bitvec::OpCodes::BvUGt.into(),
                "<" => bitvec::OpCodes::BvULt.into(),
                "=" => core::OpCodes::Cmp.into(),
                _ => panic!("Invalid"),
            };

            let op_1 = {
                if &tokens[1][0..1] == "[" {
                    let addr = {
                        let addr_ = u64::from_str_radix(&tokens[1][3..tokens[1].len() - 1], 16)
                                        .expect("Invalid integer base16");
                        ctx.define_const(addr_, 64)
                    };
                    ctx.mem_read(addr, 64)
                } else {
                    ctx.reg_read(tokens[1])
                }
            };

            let op_2 = {
                if tokens[2].len() > 2 && &tokens[2][0..2] == "0x" {
                    let const_v = u64::from_str_radix(&tokens[2][2..], 16)
                                      .expect("Invalid base16 Integer");
                    ctx.define_const(const_v, 64)
                } else {
                    ctx.reg_read(tokens[2])
                }
            };
            ctx.eval(cmd, vec![op_1, op_2]);
            self.console.print_success("Constraint Added!");
        }
    }
}

impl PathExplorer for InteractiveExplorer {
    type C = RuneControl;
    type Ctx = RuneContext<SegMem, RuneRegFile>;

    fn new() -> InteractiveExplorer {
        InteractiveExplorer {
            cmd_q: Vec::new(),
            console: Default::default(),
            single_step: false,
            bp: Vec::new(),
        }
    }

    fn next(&mut self, ctx: &mut Self::Ctx) -> RuneControl {
        if self.single_step || self.bp.contains(&ctx.ip()) {
            self.console.print_info(&format!("Halted at {:#x}", ctx.ip()));
            loop {
                self.single_step = match self.console.read_command()[0] {
                    Command::Step => true,
                    Command::Continue => false,
                    Command::DebugQuery => {
                        self.print_debug(ctx);
                        continue;
                    },
                    Command::Assertion => {
                        self.add_assertion(ctx);
                        continue;
                    },
                    Command::Query => {
                        self.query_constraints(ctx);
                        continue;
                    },
                    Command::Help => {
                        self.console.print_help();
                        continue;
                    },
                    Command::Safety => {
                        self.safety(ctx);
                        continue;
                    },
                    Command::Exit => {
                        self.console.print_info("Thanks for using rune!");
                        process::exit(1);
                    },
                    _ => continue,
                };
                break;
            }
        }
        RuneControl::Continue
    }

    fn next_job(&mut self, _ctx: &mut Self::Ctx) -> Option<RuneControl> {
        None
    }

    fn register_branch(&mut self,
                       ctx: &mut Self::Ctx,
                       condition: <Self::Ctx as RegisterRead>::VarRef)
                       -> RuneControl {
        if self.cmd_q.is_empty() {
            self.console.print_info(&format!("Encountered Branch At {:#x}", ctx.ip()));
            self.cmd_q = self.console.read_command();
        }

        if let Some(cmd) = self.cmd_q.pop() {
            match cmd {
                Command::FollowTrue => {
                    let one = ctx.define_const(1, 64);
                    ctx.eval(core::OpCodes::Cmp, &[condition, one]);
                    RuneControl::ExploreTrue
                }
                Command::FollowFalse => {
                    let zero = ctx.define_const(0, 64);
                    ctx.eval(core::OpCodes::Cmp, &[condition, zero]);
                    RuneControl::ExploreFalse
                }
                _ => panic!("Incompatible command"),
            }
        } else {
            unreachable!()
        }
    }
}
