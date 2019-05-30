//! Defines abstraction `Console`

extern crate rustyline;
use self::rustyline::error::ReadlineError;
use self::rustyline::Editor;

use std::io::{self, Write};
use std::iter;

use rune::explorer::interactive::Command;

static PROMPT: &'static str = "\x1b[1;32m>>>\x1b[0m ";
static OUTPUT: &'static str = "< ";
static ASSERT_HELP: &'static str = "
Adding assertions:
<operation> <register> <reg/const in hex>
Valid operations: =, >, <, <=, >=
";
static HELP: &'static str = "runec help menu:

Branch Follow Commands:
  t     Follow `True` branch
  f     Follow `False` branch
-------------------------------------------------------------
Interpreter Commands:
  c     Continue Execution
  s     Single Step Instruction
  r     Run
  dS    Debug State
  dQ    Debug contraints
  ?     Add Assertion
  Q     Query Constraint Solver
  x     Add safety assertions
  S     Save current state as json to 'state.json'
  h     Print Help Menu
-------------------------------------------------------------
Initial State Configuration: 
 * e <reg/mem> = <value>
   eg. E rip = 0x9000 (To set to a constant value)
       E rax = SYM (To set symbolic)
   Set register or memory to a certain value or set symbolic
 * b <addr>
   eg. b 0x9000
   Set breakpoint for the emulator
";

#[derive(Clone, Debug)]
pub struct Console {
    prompt: String,
    out_prompt: String,
    help: String,
    assert_help: String,
}

impl Default for Console {
    fn default() -> Console {
        Console {
            prompt: PROMPT.to_owned(),
            out_prompt: OUTPUT.to_owned(),
            help: HELP.to_owned(),
            assert_help: ASSERT_HELP.to_owned(),
        }
    }
}

impl Console {
    pub fn read_command(&self) -> Vec<Command> {
        let mut repeat: u32 = 1;
        let mut cmd: Command;
        let mut r = Editor::<()>::new();

        if let Err(_) = r.load_history("history.txt") {
            self.print_info("No history found.");
        }

        loop {
            // TODO: Add command completion using rustyline configurations
            let readline = r.readline(PROMPT);

            match readline {
                Ok(buffer) => {
                    r.add_history_entry(buffer.clone());
                    cmd = From::from(buffer.to_owned());

                    if cmd.is_chainable() {
                        let mut iter = buffer.split_whitespace();
                        iter.next();
                        repeat = if let Some(num) = iter.next() {
                            num.chars().fold(0, |acc, c:char| {
                                acc*10 + c.to_digit(10).unwrap()
                            })
                        } else {
                            1
                        }
                    }

                    if cmd.is_valid() {
                        break;
                    }
                },
                Err(ReadlineError::Interrupted) => {
                    repeat = 1;
                    continue;
                },
                Err(ReadlineError::Eof) => {
                    cmd = Command::Exit;
                    repeat = 1;
                    break;
                }, 
                Err(err) => {
                    println!("[!] Error: {:?}", err);
                    repeat = 1;
                    continue;
                }
            }
        }
        r.save_history("history.txt").unwrap();
        iter::repeat(cmd).take(repeat as usize + 1).collect::<Vec<_>>()
    }

    pub fn readline(&self) -> io::Result<String> {
        self.print_prompt();
        let mut buffer = String::new();
        let _ = io::stdin().read_line(&mut buffer);
        Ok(buffer)
    }

    pub fn print_prompt(&self) {
        print!("{}", self.prompt);
        io::stdout().flush().ok().expect("Could not flush stdout");
    }

    pub fn print_help(&self) {
        self.print_info(&self.help);
    }

    pub fn print_assertion_help(&self) {
        println!("{}", self.assert_help);
    }

    pub fn print_success(&self, s: &str) {
        println!("[$] {}", s);
    }

    pub fn print_error(&self, s: &str) {
        println!("[!] {}", s);
    }

    pub fn print_info(&self, s: &str) {
        println!("[*] {}", s);
    }
}
