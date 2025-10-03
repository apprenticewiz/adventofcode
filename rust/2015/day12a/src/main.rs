use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use regex::Regex;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<i32> {
    let re = Regex::new(r"([+-]?\d+)").unwrap();
    let mut result: i32 = 0;
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    for line_result in reader.lines() {
        let line = line_result?;
        for (_, [num]) in re.captures_iter(&line).map(|c| c.extract()) {
            result += num.parse::<i32>().unwrap();
        }
    }
    Ok(result)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let filename = &args[1];
    match process_file(filename) {
        Ok(result) => println!("result = {result}"),
        Err(e) => {
            eprintln!("error while processing file `{filename}': {e}");
            process::exit(1);
        }
    }
}
