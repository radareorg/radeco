//! Defines Commands available to the Explorer.

use crate::utils::utils::{Key, convert_to_u64, to_assignment, SAssignment, ValType};

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Command {
    FollowTrue,
    FollowFalse,
    Continue,
    Step,
    DebugQuery,
    DebugState,
    Assertion,
    Run,
    Query,
    Help,
    Save,
    Safety,
    Invalid,
    SetContext(SAssignment),
    SetVar(SAssignment),
    Exit,
}

impl Command {
    pub fn is_invalid(&self) -> bool {
        *self == Command::Invalid
    }

    pub fn is_valid(&self) -> bool {
        !self.is_invalid()
    }

    pub fn is_set(&self) -> bool {
        match *self {
            Command::SetVar(_) | Command::SetContext(_) => true,
            _ => false,
        }
    }

    pub fn is_chainable(&self) -> bool {
        !self.is_invalid() && !self.is_set()
    }
}

/* Implementation notes:
 * Maybe have StepInto and StepOver later.
 * Reserve 'e' for environment variables.
 * Add method to set memory range as symbolic
 * Have r2 style self-documentation
 */
impl From<String> for Command {
    fn from(s: String) -> Command {
        if let Some(c) = s.chars().nth(0) { 
            match c {
                // dc -> debug context
                // dq debug query
                't' => Command::FollowTrue,
                'f' => Command::FollowFalse,
                'c' => Command::Continue,
                's' => Command::Step,
                'b' => {
                    let (_, addr) = s.split_at(2);
                    if let Some(val) = convert_to_u64(addr.trim()) {
                        Command::SetContext(SAssignment {
                            lvalue: Key::Mem(val as usize),
                            rvalue: ValType::Break,
                        })
                    } else {
                        Command::Invalid
                    }
                }
                'd' => {
                    if let Some(c2) = s.chars().nth(1) {
                        match c2 {
                            'S' => Command::DebugState,
                            'Q' => Command::DebugQuery,
                            _ => Command::Invalid,
                        } 
                    } else {
                        Command::Invalid
                    }
                },
                '?' => Command::Assertion,
                'Q' => Command::Query,
                'e' => {
                    let (_, cmd) = s.split_at(2);
                    if let Some(val) = to_assignment(cmd) {
                        Command::SetContext(val)
                    } else {
                        Command::Invalid
                    }
                }
                'S' => Command::Save,
                'h' => Command::Help,
                'r' => Command::Run,
                'x' => Command::Safety,
                _ => Command::Invalid,
            }
        } else {
            Command::Invalid
        }
    }
}
