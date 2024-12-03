use regex::Regex;
use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn process(contents: &str) -> u32 {
    let mut result = 0;
    let mut enabled = true;
    let insn_re = Regex::new(r"(mul\([0-9]+,[0-9]+\)|don't|do)").unwrap();
    let mul_re = Regex::new(r"mul\(([0-9]+),([0-9]+)\)").unwrap();
    for (_, [insn]) in insn_re.captures_iter(contents).map(|c| c.extract()) {
        match insn {
            "don't" => enabled = false,
            "do" => enabled = true,
            _ => {
                if enabled {
                    for (_, [first_str, second_str]) in mul_re.captures_iter(insn).map(|c| c.extract()) {
                        let first = first_str.parse::<u32>().unwrap();
                        let second = second_str.parse::<u32>().unwrap();
                        result += first * second;
                    }
                }
            }
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
