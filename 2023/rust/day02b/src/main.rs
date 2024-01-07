use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn process(contents: &str) -> u32 {
    let mut result: u32 = 0;
    for line in contents.lines() {
        if let Some((_, reveals_str)) = line.split_once(": ") {
            let mut red_needed: u32 = 0;
            let mut green_needed: u32 = 0;
            let mut blue_needed: u32 = 0;
            for subset_str in reveals_str.split("; ") {
                for cubes_str in subset_str.split(", ") {
                    if let Some((amount_str, color)) = cubes_str.split_once(' ') {
                        let amount: u32 = amount_str.parse::<u32>().unwrap();
                        match color {
                            "red" => {
                                if amount > red_needed {
                                    red_needed = amount;
                                }
                            }
                            "green" => {
                                if amount > green_needed {
                                    green_needed = amount;
                                }
                            }
                            "blue" => {
                                if amount > blue_needed {
                                    blue_needed = amount;
                                }
                            }
                            _ => panic!("unknown color"),
                        }
                    }
                }
            }
            result += red_needed * green_needed * blue_needed;
        }
    }
    result
}

fn main() {
    if env::args().count() < 2 {
        usage();
    }
    let filename = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(filename).expect("read of input file failed");
    let result = process(&contents);
    println!("result = {result}");
}
