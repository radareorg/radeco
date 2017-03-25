use clap::{Arg, App};
use std::collections::HashSet;

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
        .help("-f --functions Function names to analyze")
}

pub fn print_match_summary(matched_funcs: &Vec<(u64, &String)>,
                           requested_funcs: &Vec<String>,
                           all_func_names: &Vec<&String>) {

    // Tells the user if a partial match happened
    if requested_funcs.len() == matched_funcs.len() {
        println!("All requested functions were found");
        return;
    }

    if requested_funcs.len() > matched_funcs.len() && matched_funcs.len() > 0 {
        println!("Some requested functions weren't found: ");

        let func_names: HashSet<&String> = matched_funcs.iter().map(|rfn| rfn.1).collect();
        let requested_funcs: HashSet<&String> = requested_funcs.iter().collect();

        let not_found = requested_funcs.difference(&func_names);

        for func_name in not_found {
            print!("{} ", func_name);
        }

        println!("");
        return;
    }

    if matched_funcs.len() == 0 {
        println!("None of the requested functions were found, showing printing all function names");
        for name in all_func_names {
            println!("{}", *name);
        }
    }


    // Function names found by Radeco, print those incase nothing was matched
    //let all_func_names: Vec<&String> = rmod.functions.values().map(|&ref rfn| &rfn.name).collect();

}