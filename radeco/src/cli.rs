use clap::{App, Arg};
use std::process;

pub fn parse_args() -> (Option<String>, Option<String>, bool, bool, bool) {
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
    (bin, command, is_append, is_batch, no_highlight)
}
