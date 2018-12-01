use clap::{Arg, App};
use std::process;

pub fn parse_args() -> (Option<String>, bool, bool, bool) {
    let vs = env!("VERSION_STR");
    let matches = App::new("radeco").version(vs)
                .arg(Arg::with_name("BIN")
                    .help("Binary to load")
                    .required(false))
                .arg(Arg::from_usage("-a --append 'Append separator to the end of every output.'"))
                .arg(Arg::from_usage("-b --batch 'Decompile the whole binary'"))
                .arg(Arg::from_usage("-l --highlight 'Use syntax highlight on output'"))
                .get_matches();
    let is_append = matches.is_present("append");
    let is_batch = matches.is_present("batch");
    let is_highlight = matches.is_present("highlight");
    let bin = matches.value_of("BIN").map(|s| s.to_string());
    if is_batch && bin.is_none() {
        eprintln!("Pass a binary for batch mode");
        process::exit(0);
    }
    (bin, is_append, is_batch, is_highlight)
}
