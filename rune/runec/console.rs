//! Defines abstraction `Console`

extern crate rustyline;
use self::rustyline::error::ReadlineError;
use self::rustyline::history::FileHistory;
use self::rustyline::Editor;

use std::io::{self, Write};

use rune::explorer::interactive::Command;

static PROMPT: &str = "\x1b[1;32m>>>\x1b[0m ";
static OUTPUT: &str = "< ";
static ASSERT_HELP: &str = "
Adding assertions:
<operation> <register> <reg/const in hex>
Valid operations: =, >, <, <=, >=
";
static HELP: &str = "runec help menu:

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

/// Represents the error variants for the runec console.
#[derive(Debug)]
pub enum ConsoleError {
    Rustyline,
    HistoryLoad,
    HistorySave,
    FlushStdOut,
}

impl std::fmt::Display for ConsoleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rustyline => write!(f, "console: unable to create rustyline Editor"),
            Self::HistoryLoad => write!(f, "console: unable to load console history"),
            Self::HistorySave => write!(f, "console: unable to save console history"),
            Self::FlushStdOut => write!(f, "console: unable to flush std out"),
        }
    }
}

impl std::error::Error for ConsoleError {}

impl From<ConsoleError> for io::Error {
    fn from(err: ConsoleError) -> Self {
        Self::other(err.to_string())
    }
}

/// Convenience alias for the runec console.
pub type ConsoleResult<T> = std::result::Result<T, ConsoleError>;

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
    pub fn read_command(&self) -> ConsoleResult<Vec<Command>> {
        let mut repeat: u32 = 1;
        let mut cmd: Command;
        let mut r = Editor::<(), FileHistory>::new().map_err(|_| ConsoleError::Rustyline)?;

        if r.load_history("history.txt").is_err() {
            self.print_info(format!("{}: No history found.", ConsoleError::HistoryLoad).as_str());
        }

        loop {
            // TODO: Add command completion using rustyline configurations
            let readline = r.readline(PROMPT);

            match readline {
                Ok(buffer) => {
                    if let Err(err) = r.add_history_entry(buffer.clone()) {
                        println!("(!) Error adding history entry: {err}");
                        continue;
                    }
                    cmd = From::from(buffer.to_owned());

                    if cmd.is_chainable() {
                        let mut iter = buffer.split_whitespace();
                        iter.next();
                        repeat = if let Some(num) = iter.next() {
                            num.chars().fold(0, |acc, c: char| {
                                acc * 10 + c.to_digit(10).unwrap_or_default()
                            })
                        } else {
                            1
                        }
                    }

                    if cmd.is_valid() {
                        break;
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    repeat = 1;
                    continue;
                }
                Err(ReadlineError::Eof) => {
                    cmd = Command::Exit;
                    repeat = 1;
                    break;
                }
                Err(err) => {
                    if !self.out_prompt.is_empty() {
                        self.print_out_prompt()?;
                    }
                    println!("[!] Error: {err:?}");
                    repeat = 1;
                    continue;
                }
            }
        }

        r.save_history("history.txt")
            .map_err(|_| ConsoleError::HistorySave)?;

        Ok(std::iter::repeat_n(cmd, repeat as usize + 1).collect::<Vec<_>>())
    }

    pub fn readline(&self) -> io::Result<String> {
        self.print_prompt()?;
        let mut buffer = String::new();
        let _ = io::stdin().read_line(&mut buffer);
        Ok(buffer)
    }

    pub fn print_prompt(&self) -> ConsoleResult<()> {
        print!("{}", self.prompt);
        io::stdout().flush().map_err(|_| ConsoleError::FlushStdOut)
    }

    pub fn print_out_prompt(&self) -> ConsoleResult<()> {
        print!("{}", self.out_prompt);
        io::stdout().flush().map_err(|_| ConsoleError::FlushStdOut)
    }

    pub fn print_help(&self) {
        self.print_info(&self.help);
    }

    pub fn print_assertion_help(&self) {
        println!("{}", self.assert_help);
    }

    pub fn print_success(&self, s: &str) {
        println!("[$] {s}");
    }

    pub fn print_error(&self, s: &str) {
        println!("[!] {s}");
    }

    pub fn print_info(&self, s: &str) {
        println!("[*] {s}");
    }
}
