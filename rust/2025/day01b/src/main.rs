use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;
use std::time::Instant;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<i64> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut dial: i64 = 50;
    let mut zeros: i64 = 0;
    for line_result in reader.lines() {
        let line = line_result?;
        let dir = line.chars().next().unwrap();
        let amt_str = &line[1..];
        let delta = match dir {
            'L' => -amt_str.parse::<i64>().unwrap(),
            'R' => amt_str.parse::<i64>().unwrap(),
            _ => 0
        };
        let steps = delta.abs();
        let mut dist = if delta > 0 {
            (100 - dial) % 100
        } else {
            dial % 100
        };
        if dist == 0 {
            dist = 100;
        }
        if steps >= dist {
            zeros += 1 + (steps - dist) / 100;
        }
        dial = (dial + delta).rem_euclid(100);
    }
    Ok(zeros)
}

fn display_duration(duration: u128) {
    print!("elapsed time: ");
    let duration_f = duration as f64;
    if duration_f < 1000.0 {
        println!("{} ns", duration_f);
    } else if duration_f < 1000000.0 {
        println!("{} μs", duration_f / 1000.0);
    } else if duration_f < 1000000000.0 {
        println!("{} ms", duration_f / 1000000.0);
    } else {
        println!("{} s", duration_f / 1000000000.0);
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
