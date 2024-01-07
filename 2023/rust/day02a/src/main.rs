use std::env;
use std::fs;
use std::process;

const TOTAL_RED: u32 = 12;
const TOTAL_GREEN: u32 = 13;
const TOTAL_BLUE: u32 = 14;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn process(contents: &str) -> u32 {
    let mut result: u32 = 0;
    for line in contents.lines() {
        if let Some((game_str, reveals_str)) = line.split_once(": ") {
            if let Some((_, game_num_str)) = game_str.split_once(' ') {
                let game_num: u32 = game_num_str.parse::<u32>().unwrap();
                let mut valid: bool = true;
                for subset_str in reveals_str.split("; ") {
                    for cubes_str in subset_str.split(", ") {
                        if let Some((amount_str, color)) = cubes_str.split_once(' ') {
                            let amount: u32 = amount_str.parse::<u32>().unwrap();
                            match color {
                                "red" => {
                                    if amount > TOTAL_RED {
                                        valid = false;
                                    }
                                }
                                "green" => {
                                    if amount > TOTAL_GREEN {
                                        valid = false;
                                    }
                                }
                                "blue" => {
                                    if amount > TOTAL_BLUE {
                                        valid = false;
                                    }
                                }
                                _ => panic!("unknown color"),
                            }
                        }
                    }
                }
                if valid {
                    result += game_num;
                }
            }
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
