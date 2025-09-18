use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<u32> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut result: u32 = 0;
    for line_result in reader.lines() {
        let line = line_result?;
        let code_len = line.len() as u32;
        let mut mem_len = 0;
        let mut i: usize = 1;
        while i < line.len() - 1 {
            match line.chars().nth(i).unwrap() {
                '\\' => {
                    match line.chars().nth(i + 1).unwrap() {
                        '\\' | '\"' => i += 2,
                        'x' => i += 4,
                        _ => i += 1,
                    }
                }
                _ => i += 1,
            }
            mem_len += 1;
        }
        result += code_len - mem_len;
    }
    Ok(result)
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
