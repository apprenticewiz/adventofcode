use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use aoc_utils::geometry::Position2D;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<u32> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut positions: HashSet<Position2D<i32>> = HashSet::new();
    let mut santa = Position2D::<i32>::new(0, 0);
    for line_result in reader.lines() {
        let line = line_result?;
        for ch in line.chars() {
            match ch {
                '^' => santa.y += 1,
                'v' => santa.y -= 1,
                '<' => santa.x -= 1,
                '>' => santa.x += 1,
                _ => {}
            }
            positions.insert(santa);
        }
    }
    Ok(positions.len() as u32)
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
