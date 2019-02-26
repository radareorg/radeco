use clap::{App, Arg};
use std::process;

use super::MAX_ITERATIONS;

pub fn parse_args() -> (Option<String>, Option<String>, bool, bool, bool, u32) {
    let vs = env!("VERSION_STR");
    let matches = App::new("radeco")
        .version(vs)
        .arg(Arg::with_name("BIN").help("Binary to load").required(false))
        .arg(Arg::with_name("command")
            .help("Run a custom command in batch mode")
            .short("c")
            .long("command")
            .required(false)
            .takes_value(true))
        .arg(Arg::with_name("max-iterations")
            .help("Max number of iterations of the engine")
            .short("i")
            .long("max-iterations")
            .required(false)
            .takes_value(true))
        .arg(Arg::from_usage(
            "-a --append 'Append separator to the end of every output.'",
        ))
        .arg(Arg::from_usage("-b --batch 'Decompile the whole binary'"))
        .arg(Arg::from_usage(
            "-l --no-highlight 'Disable syntax highlight on output'",
        ))
        .get_matches();
    let is_append = matches.is_present("append");
    let is_batch = matches.is_present("batch");
    let no_highlight = matches.is_present("no-highlight");
    let bin = matches.value_of("BIN").map(|s| s.to_string());
    let command = matches.value_of("command").map(|s| s.to_string());

    if is_batch && bin.is_none() {
        eprintln!("Pass a binary for batch mode");
        process::exit(0);
    }
    if command.is_some() && !is_batch {
        eprintln!("Passed a command in interactive mode");
        process::exit(0);
    }
    let max_it = match matches.value_of("max-iterations") {
        Some(s) => {
            // TODO -> Implement error management.
            match u32::from_str_radix(s.trim(), 10) {
                Ok(max_it) => max_it,
                Err(_) => {
                    eprintln!("max-iterations must be a deciamal number");
                    process::exit(0);
                },
            }
        },
        None => MAX_ITERATIONS,
    };

    if max_it == 0 {
        eprintln!("max-iterations can't be zero");
        process::exit(0);
    }

    (bin, command, is_append, is_batch, no_highlight, max_it)
}
