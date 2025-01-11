use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn parse_input(contents: &str) -> Vec<(u64, Vec<u64>)> {
    contents.lines().fold(vec![], |mut acc, line| {
        let nums: Vec<&str> = line.split_whitespace().collect();
        if let Some(first) = nums.first() {
            let result: u64 = first
                .chars()
                .filter(|c| c.is_ascii_digit())
                .collect::<String>()
                .parse::<u64>()
                .unwrap_or(0);
            let operands: Vec<u64> = nums
                .iter()
                .skip(1)
                .filter_map(|s| s.parse::<u64>().ok())
                .rev()
                .collect();
            acc.push((result, operands));
        }
        acc
    })
}

fn valid_expr((total, operands): (u64, &[u64])) -> bool {
    if operands.len() == 1 {
        operands[0] == total
    } else {
        let head = operands[0];
        let tail = &operands[1..];
        let can_div = total % head == 0;
        let can_sub = total >= head;
        (can_div && valid_expr((total / head, tail)))
            || (can_sub && valid_expr((total - head, tail)))
    }
}

fn process(contents: &str) -> u64 {
    let input = parse_input(contents);
    input
        .into_iter()
        .filter(|(result, operands)| valid_expr((*result, operands.as_slice())))
        .map(|(total, _)| total)
        .sum()
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
