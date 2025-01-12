use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

const COUNT: usize = 25;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn parse_input(contents: &str) -> HashMap<u64, u64> {
    let mut stones: HashMap<u64, u64> = HashMap::new();
    for stone in contents.split_whitespace() {
        stones.insert(stone.parse::<u64>().unwrap(), 1);
    }
    stones
}

fn blink(stones: &HashMap<u64, u64>) -> HashMap<u64, u64> {
    let mut new_stones: HashMap<u64, u64> = HashMap::new();
    for (stone, count) in stones.iter() {
        if *stone == 0 {
            new_stones
                .entry(1)
                .and_modify(|e| *e += *count)
                .or_insert(*count);
        } else {
            let len = stone.to_string().len() as u32;
            if len % 2 == 0 {
                let first_half = stone / (10_u64.pow(len / 2));
                let second_half = stone % (10_u64.pow(len / 2));
                new_stones
                    .entry(second_half)
                    .and_modify(|e| *e += *count)
                    .or_insert(*count);
                new_stones
                    .entry(first_half)
                    .and_modify(|e| *e += *count)
                    .or_insert(*count);
            } else {
                new_stones
                    .entry(*stone * 2024)
                    .and_modify(|e| *e += *count)
                    .or_insert(*count);
            }
        }
    }
    new_stones
}

fn process(contents: &str) -> u64 {
    let mut stones = parse_input(contents);
    for _ in 0..COUNT {
        stones = blink(&stones);
    }
    stones.values().sum()
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
