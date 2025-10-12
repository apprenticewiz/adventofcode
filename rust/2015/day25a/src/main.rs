use std::env;
use std::fs;
use std::io;
use std::process;

use regex::Regex;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn generate(row: usize, col: usize) -> i64 {
    let mut n = 20151125;
    let mut r = row;
    let mut c = col;
    while !(r == 1 && c == 1) {
        if r >= 1 && c == 1 {
            c = r - 1;
            r = 1;
        } else {
            r += 1;
            c -= 1;
        }
        n = (n * 252533) % 33554393;
    }
    n
}

fn process_file(filename: &str) -> io::Result<i64> {
    let content = fs::read_to_string(filename)?;
    let re = Regex::new(r"To continue, please consult the code grid in the manual.  Enter the code at row (\d+), column (\d+).").unwrap();
    let caps = re.captures(&content).expect("malformed input");
    let row = caps.get(1).unwrap().as_str().parse::<usize>().unwrap();
    let col = caps.get(2).unwrap().as_str().parse::<usize>().unwrap();
    Ok(generate(row, col))
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
