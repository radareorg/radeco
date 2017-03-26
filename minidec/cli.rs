extern crate docopt;

use std::collections::HashSet;

// Create Docopt parser and fetches the CLI arguments as appropriate
// Returns an empty vector if no args were found
pub fn init_for_args(usage: &str) -> Vec<String> {

    let args = docopt::Docopt::new(usage).and_then(|d| d.parse());

    let mut arg_vect: Vec<String> = Vec::new();
    if let Ok(ref arg_map) = args {
        arg_vect = arg_map.get_vec("<names>").iter().map(|&slice| String::from(slice)).collect()
    }
    arg_vect
}

// Prints summary of the matching if any command line arguments were
// specified,
//
// In case of full match, a message will be displayed to the user
// In case of partial matching, a list of unmatched items will be shown
// If no matching occurred, all the available results will be displayed
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
}