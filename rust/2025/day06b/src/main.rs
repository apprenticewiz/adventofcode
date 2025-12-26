use std::env;
use std::fs;
use std::io;
use std::process;
use std::time::Instant;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn compute_result(numbers: &[i64], operator: char) -> i64 {
    if operator == '+' {
        numbers.iter().sum()
    } else {
        numbers.iter().product()
    }
}

fn process_file(filename: &str) -> io::Result<i64> {
    let contents = fs::read_to_string(filename)?;
    let grid: Vec<&[u8]> = contents.lines().map(|l| l.as_bytes()).collect();
    let max_len = grid.iter().map(|l| l.len()).max().unwrap_or(0);
    let mut result: i64 = 0;
    let mut numbers: Vec<i64> = Vec::new();
    let mut operator: Option<char> = None;
    for col in (0..max_len).rev() {
        let is_all_spaces = grid.iter().all(|line| col >= line.len() || line[col] == b' ');
        if is_all_spaces {
            if let Some(op) = operator {
                result += compute_result(&numbers, op);
                numbers.clear();
                operator = None;
            }
            continue;
        }
        let mut num: i64 = 0;
        let mut multiplier: i64 = 1;
        for line in grid.iter().rev() {
            let byte = if col < line.len() { line[col] } else { b' ' };
            if byte >= b'0' && byte <= b'9' {
                num += ((byte - b'0') as i64) * multiplier;
                multiplier *= 10;
            } else if byte == b'+' || byte == b'*' {
                operator = Some(byte as char);
            }
        }
        if multiplier > 1 {
            numbers.push(num);
        }
    }
    if let Some(op) = operator {
        result += compute_result(&numbers, op);
    }
    Ok(result)
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
