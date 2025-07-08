use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn is_vowel(ch: char) -> bool {
    ch == 'a' || ch == 'e' || ch == 'i' || ch == 'o' || ch == 'u'
}

fn prop1(line: &str) -> bool {
    let mut vowels: u32 = 0;
    for ch in line.chars() {
        if is_vowel(ch) {
            vowels += 1;
        }
    }
    vowels >= 3
}

fn prop2(line: &str) -> bool {
    for ch in 'a'..='z' {
        let mut double_ch = String::new();
        double_ch.push(ch);
        double_ch.push(ch);
        if line.contains(double_ch.as_str()) {
            return true;
        }
    }
    false
}

fn prop3(line: &str) -> bool {
    !line.contains("ab") && !line.contains("cd") && !line.contains("pq") && !line.contains("xy")
}

fn process_file(filename: &str) -> io::Result<u32> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut count: u32 = 0;
    for line_result in reader.lines() {
        let line = line_result?;
        if prop1(&line) && prop2(&line) && prop3(&line) {
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
