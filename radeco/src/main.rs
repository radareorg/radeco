#[cfg(feature = "trace_log")]
extern crate env_logger;

extern crate radeco_lib;
extern crate r2pipe;
extern crate r2api;
extern crate base64;
extern crate rustyline;

mod core;

use rustyline::{CompletionType, Config, EditMode, Editor};
use rustyline::completion::Completer;
use rustyline::completion::FilenameCompleter;
use rustyline::error::ReadlineError;
use std::env;
use std::fs;

mod scheme {
    pub const HTTP: &'static str = "http://";
    pub const TCP: &'static str = "tcp://";

    pub fn is_http(url: &str) -> bool {
        url.starts_with(HTTP)
    }

    pub fn is_tcp(url: &str) -> bool {
        url.starts_with(TCP)
    }
}

// On unix platforms you can use ANSI escape sequences
#[cfg(unix)]
const PROMPT: &'static str = "\x1b[1;32m>>\x1b[0m ";

// Windows consoles typically don't support ANSI escape sequences out
// of the box
#[cfg(windows)]
const PROMPT: &'static str = ">> ";

#[derive(Default)]
struct Completes {
    file_completer: FilenameCompleter,
}

impl Completer for Completes {
    fn complete(&self, line: &str, _pos: usize) -> rustyline::Result<(usize, Vec<String>)> {
        // TODO Completion for function names
        let cmds = vec![
            command::HELP,
            command::LOAD,
            command::CONNECT,
            command::FNLIST,
            command::ANALYZE,
            command::DOT,
            command::IR,
            command::DECOMPILE,
        ];
        let mut ret: Vec<String> = cmds.into_iter()
            .filter(|s| s.starts_with(line))
            .map(|s| s.to_string())
            .collect();
        match self.file_completer.complete(line, _pos) {
            Ok((n, ss)) => {
                let mut completed_lines = ss.into_iter()
                    .map(|s| format!("{}{}", &line[..n], s))
                    .collect();
                ret.append(&mut completed_lines);
            }
            Err(_) => {}
        }
        Ok((0, ret))
    }
}

fn main() {
    let config = Config::builder()
        .auto_add_history(true)
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build();
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some((Completes::default(), ())));
    core::PROJ.with(|proj| {
        *proj.borrow_mut() = env::args().nth(1).and_then(|ref s| {
            if scheme::is_http(s) {
                core::load_proj_http(&s[scheme::HTTP.len()..])
            } else if scheme::is_tcp(s) {
                core::load_proj_tcp(&s[scheme::TCP.len()..])
            } else if is_file(s) {
                Ok(core::load_proj_by_path(s))
            } else {
                Err("Invalid argument")
            }.ok()
        });
    });

    loop {
        let readline = rl.readline(PROMPT);
        match readline {
            Ok(line) => {
                let mut terms = line.split_whitespace();
                let o1 = terms.next();
                let o2 = terms.next();
                cmd(o1, o2);
                println!("END");
            }
            Err(ReadlineError::Interrupted) |
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

mod command {
    pub const HELP: &'static str = "help";
    pub const LOAD: &'static str = "load";
    pub const CONNECT: &'static str = "connect";
    pub const FNLIST: &'static str = "core::fn_list";
    pub const ANALYZE: &'static str = "analyze";
    pub const DOT: &'static str = "dot";
    pub const IR: &'static str = "ir";
    pub const DECOMPILE: &'static str = "decompile";

    pub fn help() {
        let width = 30;
        println!("{:width$}    Show this help", HELP, width = width);
        println!(
            "{:width$}    Load binary",
            format!("{} path", LOAD),
            width = width
        );
        println!(
            "{:width$}    Connect to radare2 server",
            format!("{} (http|tcp)://<url>", CONNECT),
            width = width
        );
        println!(
            "{:width$}    Show function list",
            FNLIST,
            width = width
        );
        println!(
            "{:width$}    Analyze <func>",
            format!("{} <func>", ANALYZE),
            width = width
        );
        println!(
            "{:width$}    Emit IR of <func>",
            format!("{} <func>", IR),
            width = width
        );
        println!(
            "{:width$}    Emit graph of the IR in Graphviz dot",
            format!("{} <func>", DOT),
            width = width
        );
        println!(
            "{:width$}    Decompile <func>",
            format!("{} <func>", DECOMPILE),
            width = width
        );
    }
}


fn cmd(op1: Option<&str>, op2: Option<&str>) {
    core::PROJ.with(|proj_opt| {
        match (op1, op2) {
            (Some(command::HELP), _) => {
                command::help();
                return;
            }
            (Some(command::LOAD), Some(path)) => {
                if is_file(path) {
                    *proj_opt.borrow_mut() = Some(core::load_proj_by_path(path));
                    return;
                } else {
                    println!("{} is not found.", path);
                    return;
                }
            }
            (Some(command::CONNECT), Some(url)) => {
                let p_opt = if scheme::is_http(&url) {
                    core::load_proj_http(&url[scheme::HTTP.len()..])
                } else if scheme::is_tcp(&url) {
                    core::load_proj_tcp(&url[scheme::TCP.len()..])
                } else {
                    Err("Invalid url")
                };
                match p_opt {
                    Ok(p) => *proj_opt.borrow_mut() = Some(p),
                    Err(msg) => println!("{}", msg),
                }
                return;
            }
            _ => {}
        };
        if proj_opt.borrow().is_none() {
            println!("Load a project first");
            return;
        }
        let mut proj_ = proj_opt.borrow_mut();
        let proj = proj_.as_mut().unwrap();
        match (op1, op2) {
            (Some(command::ANALYZE), Some("*")) => {
                let rfns = proj.iter_mut().map(|i| i.module).flat_map(|rmod| {
                    rmod.functions.values_mut()
                });
                for rfn in rfns {
                    core::analyze(rfn);
                }
            }
            (Some(command::FNLIST), _) => {
                let funcs = core::fn_list(&proj);
                println!("{}", funcs.join("\n"));
            }
            // TODO Show list of dependency information of analyses
            // TODO Add command for individual analyses
            (Some(command::ANALYZE), Some(f)) => {
                if let Some(rfn) = core::get_function_mut(f, proj) {
                    core::analyze(rfn);
                } else {
                    println!("{} is not found", f);
                }
            }
            (Some(command::DOT), Some(f)) => {
                if let Some(rfn) = core::get_function(f, &proj) {
                    println!("{}", core::emit_dot(rfn.ssa()));
                } else {
                    println!("{} is not found", f);
                }
            }
            (Some(command::IR), Some(f)) => {
                if let Some(rfn) = core::get_function(f, &proj) {
                    println!("{}", core::emit_ir(rfn));
                } else {
                    println!("{} is not found", f);
                }
            }
            (Some(command::DECOMPILE), Some(f)) => {
                if let Some(rfn) = core::get_function(f, &proj) {
                    let rmod = proj.iter().map(|i| i.module).next().unwrap();
                    let func_name_map = core::func_names(&rmod);
                    let strings = core::strings(&rmod);
                    match core::decompile(rfn, &func_name_map, &strings) {
                        Ok(res) => println!("{}", res),
                        Err(err) => println!("{}", err),
                    }
                } else {
                    println!("{} is not found", f);
                }
            }
            _ => {
                println!(
                    "Invalid command {} {}",
                    op1.unwrap_or(""),
                    op2.unwrap_or("")
                );
            }
        }
    });
}

fn is_file(path: &str) -> bool {
    fs::metadata(path).map(|f| f.is_file()).unwrap_or(false)
}
