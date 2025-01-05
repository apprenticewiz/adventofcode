use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_lists(contents: &str) -> (Vec<u32>, Vec<u32>) {
    let mut first_list = vec![];
    let mut second_list = vec![];
    for line in contents.lines() {
        let first_number = line
            .split_whitespace()
            .next()
            .unwrap()
            .parse::<u32>()
            .unwrap();
        let second_number = line
            .split_whitespace()
            .nth(1)
            .unwrap()
            .parse::<u32>()
            .unwrap();
        first_list.push(first_number);
        second_list.push(second_number);
    }
    first_list.sort();
    second_list.sort();
    (first_list, second_list)
}

fn process(contents: &str) -> u32 {
    let mut result = 0;
    let (first_list, second_list) = build_lists(contents);
    for first in first_list.iter() {
        result += *first * second_list.iter().filter(|&n| *n == *first).count() as u32;
    }
    result
}

fn calc_runtime(start_time: &SystemTime) -> String {
    let mut elapsed = start_time.elapsed().unwrap().as_micros() as f64;
    let mut units = "μs";
    if elapsed > 1000.0 {
        elapsed /= 1000.0;
        units = "ms";
    }
    if elapsed > 1000.0 {
        elapsed /= 1000.0;
        units = "s";
    }
    format!("(elapsed time: {elapsed} {units})")
}

fn main() {
    let start_time = SystemTime::now();
    if env::args().count() < 2 {
        usage();
    }
    let filename = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(filename).expect("read of input file failed");
    let result = process(&contents);
    let duration = calc_runtime(&start_time);
    println!("result = {result}  {duration}");
}
