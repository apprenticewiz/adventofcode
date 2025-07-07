use std::env;
use std::io;
use std::process;

use aoc_utils::md5;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <key>");
    process::exit(1);
}

fn process_input(key: &str) -> io::Result<u32> {
    let mut n: u32 = 1;
    loop {
        let mut try_key = String::from(key);
        try_key.push_str(format!("{n}").as_str());
        let digest = md5::digest(try_key.as_str());
        if digest.as_str().starts_with("000000") {
            return Ok(n);
        }
        n += 1;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let key = &args[1];
    match process_input(key) {
        Ok(result) => println!("result = {result}"),
        Err(e) => {
            eprintln!("error while processing input: {e}");
            process::exit(1);
        }
    }
}
