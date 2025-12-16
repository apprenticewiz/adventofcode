use std::env;
use std::fs;
use std::io;
use std::process;
use std::time::Instant;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<i64> {
    let contents = fs::read_to_string(filename)?;
    let mut result: i64 = 0;
    const NUM_DIGITS: usize = 2;
    for line in contents.lines() {
        let bytes = line.as_bytes();
        let len = bytes.len();
        let mut digits = Vec::with_capacity(NUM_DIGITS);
        let mut start_idx = 0;
        for i in 0..NUM_DIGITS {
            let end_idx = len - (NUM_DIGITS - i - 1);
            if start_idx >= end_idx {
                break;
            }
            let slice = &bytes[start_idx..end_idx];
            let &max_byte = slice.iter().max().unwrap();            
            let rel_idx = slice.iter().position(|&b| b == max_byte).unwrap();
            let digit = (max_byte - b'0') as i64;
            digits.push(digit);
            start_idx += rel_idx + 1;
        }
        let number = digits.iter().fold(0i64, |acc, &d| acc * 10 + d);
        result += number;
    }
    Ok(result)
}


fn display_duration(duration: u128) {
    print!("elapsed time: ");
    let duration_f = duration as f64;
    if duration_f < 1_000.0 {
        println!("{} ns", duration_f);
    } else if duration_f < 1_000_000.0 {
        println!("{} μs", duration_f / 1_000.0);
    } else if duration_f < 1_000_000_000.0 {
        println!("{} ms", duration_f / 1_000_000.0);
    } else {
        println!("{} s", duration_f / 1_000_000_000.0);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let filename = &args[1];
    let start = Instant::now();
    match process_file(filename) {
        Ok(result) => {
            let duration = start.elapsed().as_nanos();
            println!("result = {result}");
            display_duration(duration);
        }
        Err(e) => {
            eprintln!("error while processing file `{filename}': {e}");
            process::exit(1);
        }
    }
}
