use std::env;
use std::io;
use std::process;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <string>");
    process::exit(1);
}

fn increment(s: &str) -> String {
    let mut chars: Vec<char> = s.chars().rev().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == 'z' {
            chars[i] = 'a';
            i += 1;
        } else {
            chars[i] = ((chars[i] as u8) + 1) as char;
            return chars.into_iter().rev().collect();
        }
    }

    chars.push('a');
    chars.into_iter().rev().collect()
}

fn prop1(s: &str) -> bool {
    let bytes = s.as_bytes();
    for i in 0..(bytes.len() - 2) {
        if (bytes[i + 1] == bytes[i] + 1) && (bytes[i + 2] == bytes[i + 1] + 1) {
            return true;
        }
    }
    false
}

fn prop2(s: &str) -> bool {
    s.chars().all(|c| c != 'i' && c != 'o' && c != 'l')
}

fn prop3(s: &str) -> bool {
    let chars: Vec<char> = s.chars().collect();
    let mut pairs = 0;
    let mut i = 0;

    while i < chars.len() - 1 {
        if chars[i] == chars[i + 1] {
            pairs += 1;
            i += 2;
        } else {
            i += 1;
        }
    }

    pairs >= 2
}

fn process_input(s: &str) -> io::Result<String> {
    let mut new_password = increment(s);
    while !prop1(&new_password) || !prop2(&new_password) || !prop3(&new_password) {
        new_password = increment(&new_password);
    }
    Ok(new_password)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let s = &args[1];
    match process_input(s) {
        Ok(result) => println!("result = {result}"),
        Err(e) => {
            eprintln!("error while processing input: {e}");
            process::exit(1);
        }
    }
}
