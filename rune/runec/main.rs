//! runec - Rune Console.
//!
//! Interactive shell that uses rune for user guided symbolic execution and
//! binary reasoning.

extern crate docopt;
extern crate libsmt;
extern crate r2api;
extern crate r2pipe;
extern crate rune;

mod console;
mod interact;

use docopt::Docopt;
use std::process::exit;

use serde::{Deserialize, Serialize};

use rune::engine::rune::Rune;
use rune::engine::Engine;
use rune::explorer::interactive::Command;
use rune::explorer::PathExplorer;
use rune::utils::state::RInitialState;
use rune::utils::{Key, SAssignment, ValType};

use crate::console::Console;
use crate::interact::InteractiveExplorer;

use r2api::api_trait::R2PApi;
use r2pipe::r2::R2;

static USAGE: &str = "
runec. Interactive console for rune.

Usage:
  runec [-p <path>] <file>
  runec (-h | --help)

Options:
  -h --help                              Show this screen.
  -p                                     Load a previous configuration of state
";

#[derive(Debug, Clone, Deserialize, Serialize)]
#[allow(unused)]
struct Args {
    flag_help: bool,
    flag_project: bool,
    arg_path: Option<String>,
    arg_file: Option<String>,
}

fn main() {
    let args = Docopt::new(USAGE)
        .and_then(|dopt| dopt.parse())
        .unwrap_or_else(|e| e.exit());

    if args.get_bool("-h") {
        println!("{}", USAGE);
        exit(0);
    }

    let mut stream = R2::new(Some(args.get_str("<file>"))).expect("Unable to spawn r2");
    stream.init().expect("Unable to initialize the stream");

    let _lreginfo = stream
        .reg_info()
        .expect("Unable to retrieve register info.");

    let c: Console = Default::default();
    let mut is: RInitialState = RInitialState::new();

    if args.get_bool("-p") {
        let path = args.get_str("<path>");
        match RInitialState::import_from_json(path) {
            Ok(i) => is = i,
            Err(err) => {
                c.print_error(format!("unable to load initial state: {err}").as_str());
                exit(1);
            }
        };
    }

    loop {
        match c.read_command().map(|c| c.first().cloned()) {
            Ok(Some(Command::Run)) => {
                // NOTE: This allows us to use any explorer here.
                let mut explorer = InteractiveExplorer::new();
                explorer.bp = is.get_breakpoints();

                let ctx = match is.create_context(&mut stream) {
                    Ok(c) => c,
                    Err(err) => {
                        c.print_error(format!("error creating context: {err}").as_str());
                        exit(1);
                    }
                };

                let mut rune = Rune::new(ctx, explorer, stream);
                rune.run().expect("Rune Error!");
                break;
            }
            Ok(Some(Command::Help)) => {
                c.print_help();
                continue;
            }
            Ok(Some(Command::Save)) => {
                if let Err(err) = is.write_to_json() {
                    c.print_error(err.to_string().as_str());
                }
                continue;
            }
            Ok(Some(Command::DebugState)) => {
                // TODO: It would be better if we pretty print the debug message.
                if let Err(err) = is.get_string().as_deref().map(|istr| c.print_info(istr)) {
                    c.print_error(err.to_string().as_str());
                }
                continue;
            }
            Ok(Some(Command::SetContext(SAssignment {
                lvalue: Key::Mem(val),
                rvalue: ValType::Break,
            }))) => {
                is.add_breakpoint(val as u64);
            }
            Ok(Some(Command::SetContext(SAssignment {
                lvalue: ref val,
                rvalue: ValType::Symbolic,
            }))) => {
                is.add_sym(val.clone());
            }
            Ok(Some(Command::SetContext(SAssignment {
                lvalue: ref key,
                rvalue: ValType::Concrete(val),
            }))) => {
                // If the register to be set is rip, we infer that the user is setting
                // their start address
                if *key == Key::Reg("rip".to_owned()) {
                    is.set_start_addr(val as u64);
                } else {
                    is.add_const((key.clone(), val as u64));
                }
            }
            Ok(Some(Command::Exit)) => {
                c.print_info("Thanks for using rune!");
                exit(0);
            }
            Ok(Some(Command::Invalid)) => {
                c.print_error("Invalid command. Please try again.");
            }
            _ => continue,
        }
    }
}
