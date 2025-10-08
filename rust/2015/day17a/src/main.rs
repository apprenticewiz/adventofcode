use std::env;
use std::fs;
use std::io;
use std::process;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<u32> {
    let mut count: u32 = 0;
    let containers: Vec<u32> = fs::read_to_string(filename)
        .unwrap()
        .lines()
        .map(|line| line.parse::<u32>().unwrap())
        .collect();
    let n = containers.len();
    for i in 0..(1 << n) {
        let mut sum = 0;
        for (j, item) in containers.iter().enumerate().take(n) {
            if (i & (1 << j)) != 0 {
                sum += item;
            }
        }
        if sum == 150 {
            count += 1;
        }
    }
    Ok(count)
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
