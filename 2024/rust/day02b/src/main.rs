use itertools::Itertools;
use std::env;
use std::fs;
use std::process;

enum Direction {
    Unknown,
    Increasing,
    Decreasing,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn check_levels(numbers: &[u32]) -> bool {
    let mut valid = true;
    let mut dir = Direction::Unknown;
    for pair in numbers.windows(2) {
        let x = pair[0];
        let y = pair[1];
        if x == y || x.abs_diff(y) > 3 {
            valid = false;
            break;
        }
        match dir {
            Direction::Unknown => {
                if y > x {
                    dir = Direction::Increasing;
                } else {
                    dir = Direction::Decreasing;
                }
            }
            Direction::Increasing => {
                if y < x {
                    valid = false;
                    break;
                }
            }
            Direction::Decreasing => {
                if y > x {
                    valid = false;
                    break;
                }
            }
        }
    }
    valid
}

fn check_levels_with_retries(numbers: &[u32]) -> bool {
    if check_levels(numbers) {
        true
    } else {
        let n = numbers.len();
        let subsequences: Vec<Vec<u32>> = numbers.iter().cloned().combinations(n - 1).collect();
        for subsequence in subsequences {
            if check_levels(subsequence.as_slice()) {
                return true;
            }
        }
        false
    }
}

fn process(contents: &str) -> u32 {
    let mut result = 0;
    for line in contents.lines() {
        let numbers: Vec<u32> = line
            .split_whitespace()
            .map(|x| x.parse::<u32>().unwrap())
            .collect();
        let valid = check_levels_with_retries(&numbers);
        if valid {
            result += 1;
        }
    }
    result
}

fn main() {
    if env::args().count() < 2 {
        usage();
    }
    let filename = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(filename).expect("read of input file failed");
    let result = process(&contents);
    println!("result = {result}");
}
