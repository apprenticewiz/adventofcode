use regex::Regex;
use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn process(contents: &str) -> u32 {
    let mut result = 0;
    let mul_re = Regex::new(r"mul\(([0-9]+),([0-9]+)\)").unwrap();
    for (_, [first_str, second_str]) in mul_re.captures_iter(contents).map(|c| c.extract()) {
        let first = first_str.parse::<u32>().unwrap();
        let second = second_str.parse::<u32>().unwrap();
        result += first * second;
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
