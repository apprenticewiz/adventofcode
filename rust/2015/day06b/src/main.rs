use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use regex::Regex;

const MAX_ROW: usize = 1000;
const MAX_COL: usize = 1000;

struct Grid {
    grid: Vec<Vec<u32>>,
}

impl Grid {
    fn new() -> Self {
        let grid = vec![vec![0; MAX_COL]; MAX_ROW];
        Self { grid }
    }

    fn perform(&mut self, action: &str, r1: usize, c1: usize, r2: usize, c2: usize) {
        for row in r1..=r2 {
            for col in c1..=c2 {
                match action {
                    "turn on" => self.grid[row][col] += 1,
                    "turn off" => self.grid[row][col] = if self.grid[row][col] > 0 { self.grid[row][col] - 1 } else { 0 },
                    "toggle" => self.grid[row][col] += 2,
                    _ => {},
                }
            }
        }
    }

    fn sum(&self) -> u32 {
        let mut sum = 0;
        for row in 0..MAX_ROW {
            for col in 0..MAX_COL {
                sum += self.grid[row][col];
            }
        }
        sum
    }
}

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<u32> {
    let mut grid = Grid::new();
    let re = Regex::new(r"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)").unwrap();
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    for line_result in reader.lines() {
        let line = line_result?;
        if let Some(caps) = re.captures(&line) {
            let action = caps.get(1).unwrap().as_str();
            let r1 = caps.get(2).unwrap().as_str().parse::<usize>().unwrap();
            let c1 = caps.get(3).unwrap().as_str().parse::<usize>().unwrap();
            let r2 = caps.get(4).unwrap().as_str().parse::<usize>().unwrap();
            let c2 = caps.get(5).unwrap().as_str().parse::<usize>().unwrap();
            grid.perform(action, r1, c1, r2, c2);
        }
    }
    Ok(grid.sum())
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
