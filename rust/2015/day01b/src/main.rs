use std::env;
use std::fs::File;
use std::io::{self, Error, BufRead};
use std::process;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<u32> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut counter: i32 = 0;
    for line_result in reader.lines() {
        let line = line_result?;
        for (i, ch) in line.chars().enumerate() {
            match ch {
                '(' => counter += 1,
                ')' => counter -= 1,
                _ => {}
            }
            if counter < 0 {
                return Ok(i as u32 + 1);
            }
        }
    }
    Err(Error::other("result was never negative"))
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
