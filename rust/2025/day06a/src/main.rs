use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;
use std::process;
use std::time::Instant;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<i64> {
    let contents = fs::read_to_string(filename)?;
    let mut result: i64 = 0;
    let mut columns: HashMap<usize, Vec<i64>> = HashMap::new();    
    for line in contents.lines() {
        let mut items = line.split_whitespace();
        let first = items.next().unwrap();
        if first.parse::<i64>().is_ok() {
            let first_num = first.parse::<i64>().unwrap();
            columns.entry(0).or_default().push(first_num);
            for (i, item) in items.enumerate() {
                let num = item.parse::<i64>().unwrap();
                columns.entry(i + 1).or_default().push(num);
            }
        } else {
            if let Some(col) = columns.get(&0) {
                result += if first == "+" {
                    col.iter().sum::<i64>()
                } else if first == "*" {
                    col.iter().product::<i64>()
                } else {
                    0
                };
            }
            for (i, op) in items.enumerate() {
                if let Some(col) = columns.get(&(i + 1)) {
                    result += if op == "+" {
                        col.iter().sum::<i64>()
                    } else if op == "*" {
                        col.iter().product::<i64>()
                    } else {
                        0
                    };
                }
            }
        }
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
