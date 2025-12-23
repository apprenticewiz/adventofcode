use std::env;
use std::fs;
use std::io;
use std::ops::RangeInclusive;
use std::process;
use std::time::Instant;

use range_set::{RangeSet, range_set};

type Ranges = RangeSet<[RangeInclusive<i64>; 4]>;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_input(contents: &str) -> Ranges {
    let mut lines = contents.lines();
    let mut ranges: Ranges = range_set![];
    for line in lines.by_ref() {
        if line.trim().is_empty() {
            break;
        }
        let (first_s, last_s) = line.split_once('-').unwrap();
        let first = first_s.parse::<i64>().unwrap();
        let last = last_s.parse::<i64>().unwrap();
        ranges = ranges.union(&range_set![first..=last]);
    }
    ranges
}

fn process_file(filename: &str) -> io::Result<i64> {
    let contents = fs::read_to_string(filename)?;
    let ranges = parse_input(&contents);
    let mut result: i64 = 0;
    for range in ranges.as_ref().iter() {
        result += *range.end() - *range.start() + 1;
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
