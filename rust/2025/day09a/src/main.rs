use std::env;
use std::fs;
use std::io;
use std::process;
use std::time::Instant;

use itertools::Itertools;

type Position = (i64, i64);

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<i64> {
    let contents = fs::read_to_string(filename)?;
    let points = contents
        .lines()
        .map(|line| {
            let parts = line.split_once(',').unwrap();
            let x = parts.0.parse::<i64>().unwrap();
            let y = parts.1.parse::<i64>().unwrap();
            (x, y)
        })
        .collect::<Vec<Position>>();
    let max_area = points
        .into_iter()
        .combinations(2)
        .map(|c| {
            let p1 = c[0];
            let p2 = c[1];
            let d1 = p1.0.abs_diff(p2.0) + 1;
            let d2 = p1.1.abs_diff(p2.1) + 1;
            d1 * d2
        })
        .max()
        .unwrap();
    Ok(max_area as i64)
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
