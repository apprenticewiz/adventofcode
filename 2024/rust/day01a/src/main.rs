use std::env;
use std::fs;
use std::process;

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
    for (first, second) in first_list.iter().zip(second_list.iter()) {
        result += first.abs_diff(*second);
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
