use std::collections::HashSet;
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
    let mut rolls = HashSet::new();
    for (row, line) in contents.lines().enumerate() {
        for (col, &byte) in line.as_bytes().iter().enumerate() {
            if byte == b'@' {
                rolls.insert((row as i64, col as i64));
            }
        }
    }
    const NEIGHBORS: [(i64, i64); 8] = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];
    let accessible = rolls
        .iter()
        .filter(|pos| {
            let (row, col) = **pos;
            let neighbor_count = NEIGHBORS
                .iter()
                .map(|(dr, dc)| (row + dr, col + dc))
                .filter(|neighbor| rolls.contains(neighbor))
                .count();
            neighbor_count < 4
        })
        .count();
    Ok(accessible as i64)
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
