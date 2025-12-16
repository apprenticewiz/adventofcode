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
    let mut dial: i64 = 50;
    let mut zeros: i64 = 0;
    for line in contents.lines() {
        if line.is_empty() {
            continue;
        }
        let bytes = line.as_bytes();
        let dir = bytes[0];
        let amt_str = &line[1..];
        let delta = match dir {
            b'L' => -amt_str.parse::<i64>().unwrap(),
            b'R' => amt_str.parse::<i64>().unwrap(),
            _ => 0,
        };
        let steps = delta.abs();
        let dist = if dial == 0 {
            100
        } else if delta > 0 {
            100 - dial
        } else {
            dial
        };
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
