use std::env;
use std::fs;
use std::io;
use std::process;
use std::time::Instant;

const POWERS_OF_10: [i64; 19] = [
    1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000,
    1_000_000_000, 10_000_000_000, 100_000_000_000, 1_000_000_000_000,
    10_000_000_000_000, 100_000_000_000_000, 1_000_000_000_000_000,
    10_000_000_000_000_000, 100_000_000_000_000_000, 1_000_000_000_000_000_000,
];

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<i64> {
    let contents = fs::read_to_string(filename)?;
    let mut result: i64 = 0;
    for range_str in contents.trim_end().split(",") {
        let (low_str, high_str) = range_str.split_once("-").unwrap();
        let low = low_str.parse::<i64>().unwrap();
        let high = high_str.parse::<i64>().unwrap();
        for num in low..=high {
            if is_invalid(num) {
                result += num;
            }
        }
    }
    Ok(result)
}

fn is_invalid(num: i64) -> bool {
    if num == 0 {
        return true;
    }
    let mut digits = 1;
    for &power in &POWERS_OF_10[1..] {
        if num < power {
            break;
        }
        digits += 1;
    }
    for pattern_len in 1..=(digits / 2) {
        if digits % pattern_len != 0 {
            continue;
        }
        let pattern_divisor = POWERS_OF_10[digits - pattern_len];
        let pattern = num / pattern_divisor;
        let repeats = digits / pattern_len;
        let mut expected = 0;
        for _ in 0..repeats {
            expected = expected * POWERS_OF_10[pattern_len] + pattern;
        }
        if expected == num {
            return true;
        }
    }
    false
}

fn display_duration(duration: u128) {
    print!("elapsed time: ");
    let duration_f = duration as f64;
    if duration_f < 1_000.0 {
        println!("{} ns", duration_f);
    } else if duration_f < 1_000_000.0 {
        println!("{} μs", duration_f / 1_000.0);
    } else if duration_f < 1_000_000_000.0 {
        println!("{} ms", duration_f / 1_000_000.0);
    } else {
        println!("{} s", duration_f / 1_000_000_000.0);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let filename = &args[1];
    let start = Instant::now();
    match process_file(filename) {
        Ok(result) => {
            let duration = start.elapsed().as_nanos();
            println!("result = {result}");
            display_duration(duration);
        }
        Err(e) => {
            eprintln!("error while processing file `{filename}': {e}");
            process::exit(1);
        }
    }
}
