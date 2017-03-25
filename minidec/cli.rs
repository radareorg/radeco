use clap::{Arg, App};
use radeco_lib::frontend::containers::RFunction;
use std::cmp::Ordering;
use std::collections::HashMap;

// Creates command line argument settings
// src: https://kbknapp.github.io/clap-rs/clap/index.html
pub fn create_args<'a, 'b>() -> App<'a, 'b>
    where 'a: 'b
{
    App::new("Radeco")
        .arg(Arg::with_name("functions")
            .short("f")
            .required(false)
            .takes_value(true)
            .multiple(true)
            .long("functions"))
        .help("Function names to analyze")
}

pub fn print_match_summary(requested_funcs_count: usize, found_funcs_count: usize) {
    
    // Tells the user if a partial match happened
    if requested_funcs_count == found_funcs_count {
        println!("All requested functions were found");
        return;
    }

    println!("Some requested functions weren't found");



    // Function names found by Radeco, print those incase nothing was matched
    //let all_func_names: Vec<&String> = rmod.functions.values().map(|&ref rfn| &rfn.name).collect();

}