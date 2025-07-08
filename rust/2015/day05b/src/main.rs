use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn prop1(line: &str) -> bool {
    for i in 0..(line.len() - 3) {
        let first = &line[i..i + 2];
        for j in i + 2..(line.len() - 1) {
            let second = &line[j..j + 2];
            if first == second {
                return true;
            }
        }
    }
    false
}

fn prop2(line: &str) -> bool {
    for i in 0..(line.len() - 2) {
        if line[i..i + 1] == line[i + 2..i + 3] {
            return true;
        }
    }
    false
}

fn process_file(filename: &str) -> io::Result<u32> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut count: u32 = 0;
    for line_result in reader.lines() {
        let line = line_result?;
        if prop1(&line) && prop2(&line) {
            count += 1;
        }
    }
    Ok(count)
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
