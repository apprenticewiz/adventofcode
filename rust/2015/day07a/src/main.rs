use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use regex::Regex;

enum Operation {
    Assign(String),
    Not(String),
    And(String, String),
    Or(String, String),
    LeftShift(String, u32),
    RightShift(String, u32),
}

type Handler = Box<dyn Fn(regex::Captures) -> (String, Operation)>;

fn usage(progname: &str) {
    eprintln!("Usage: {progname} <input_file>");
    process::exit(1);
}

fn eval(
    operations: &HashMap<String, Operation>,
    cache: &mut HashMap<String, u32>,
    expr: &str,
) -> u32 {
    if expr.parse::<u32>().is_ok() {
        expr.parse::<u32>().unwrap()
    } else if cache.contains_key(expr) {
        *cache.get(expr).unwrap()
    } else {
        let op = operations.get(expr).unwrap();
        match op {
            Operation::Assign(src) => {
                let a = eval(operations, cache, src);
                cache.insert(expr.to_string(), a);
                a
            }
            Operation::Not(src) => {
                let a = eval(operations, cache, src);
                let res = (!a) & 0xffff;
                cache.insert(expr.to_string(), res);
                res
            }
            Operation::And(src1, src2) => {
                let a = eval(operations, cache, src1);
                let b = eval(operations, cache, src2);
                let res = (a & b) & 0xffff;
                cache.insert(expr.to_string(), res);
                res
            }
            Operation::Or(src1, src2) => {
                let a = eval(operations, cache, src1);
                let b = eval(operations, cache, src2);
                let res = (a | b) & 0xffff;
                cache.insert(expr.to_string(), res);
                res
            }
            Operation::LeftShift(src, amt) => {
                let a = eval(operations, cache, src);
                let res = (a << amt) & 0xffff;
                cache.insert(expr.to_string(), res);
                res
            }
            Operation::RightShift(src, amt) => {
                let a = eval(operations, cache, src);
                let res = (a >> amt) & 0xffff;
                cache.insert(expr.to_string(), res);
                res
            }
        }
    }
}

fn process_file(filename: &str) -> io::Result<u32> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut operations: HashMap<String, Operation> = HashMap::new();
    let mut cache: HashMap<String, u32> = HashMap::new();
    let regexes: Vec<(Regex, Handler)> = vec![
        (
            Regex::new(r"^(\d+|\w+) -> (\w+)$").unwrap(),
            Box::new(|caps: regex::Captures| {
                (caps[2].to_string(), Operation::Assign(caps[1].to_string()))
            }),
        ),
        (
            Regex::new(r"NOT (\d+|\w+) -> (\w+)").unwrap(),
            Box::new(|caps: regex::Captures| {
                (caps[2].to_string(), Operation::Not(caps[1].to_string()))
            }),
        ),
        (
            Regex::new(r"(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)").unwrap(),
            Box::new(|caps: regex::Captures| match &caps[2] {
                "AND" => (
                    caps[4].to_string(),
                    Operation::And(caps[1].to_string(), caps[3].to_string()),
                ),
                "OR" => (
                    caps[4].to_string(),
                    Operation::Or(caps[1].to_string(), caps[3].to_string()),
                ),
                _ => unreachable!(),
            }),
        ),
        (
            Regex::new(r"(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)").unwrap(),
            Box::new(|caps: regex::Captures| match &caps[2] {
                "LSHIFT" => (
                    caps[4].to_string(),
                    Operation::LeftShift(caps[1].to_string(), caps[3].parse::<u32>().unwrap()),
                ),
                "RSHIFT" => (
                    caps[4].to_string(),
                    Operation::RightShift(caps[1].to_string(), caps[3].parse::<u32>().unwrap()),
                ),
                _ => unreachable!(),
            }),
        ),
    ];
    for line_result in reader.lines() {
        let line = line_result?;
        match regexes.iter().find(|(re, _)| re.is_match(&line)) {
            Some((re, f)) => {
                let caps = re.captures(&line).unwrap();
                let (dest, op) = f(caps);
                operations.insert(dest, op);
            }
            None => panic!("error: malformed input line: {line}"),
        }
    }
    Ok(eval(&operations, &mut cache, "a"))
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
