use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use regex::Regex;

type Sue = HashMap<String, u32>;

type Sues = HashMap<u32, Sue>;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_file(filename: &str) -> io::Result<Sues> {
    let mut sues: Sues = HashMap::new();
    let re = Regex::new(r"Sue (\d+): (\w+: \d+), (\w+: \d+), (\w+: \d+)").unwrap();
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    for line_result in reader.lines() {
        let line = line_result?;
        if let Some(caps) = re.captures(&line) {
            let sue_number: u32 = caps.get(1).unwrap().as_str().parse::<u32>().unwrap_or(0);
            let sue_traits: Vec<&str> = vec![
                caps.get(2).unwrap().as_str(),
                caps.get(3).unwrap().as_str(),
                caps.get(4).unwrap().as_str(),
            ];
            let mut sue: Sue = HashMap::new();
            for sue_trait in sue_traits.iter() {
                let (thing, amount_str) = sue_trait.split_once(": ").unwrap();
                let amount = amount_str.parse::<u32>().unwrap_or(0);
                sue.insert(thing.to_string(), amount);
            }
            sues.insert(sue_number, sue);
        }
    }
    Ok(sues)
}

fn is_possible(sue: &Sue, thing: &str, requested_amount: u32) -> bool {
    match sue.get(thing) {
        Some(amount) => requested_amount == *amount,
        None => true,
    }
}

fn possible_set(sues: &Sues, thing: &str, requested_amount: u32) -> HashSet<u32> {
    sues.iter()
        .filter(|(_, v)| is_possible(v, thing, requested_amount))
        .map(|(n, _)| *n)
        .collect()
}

fn process_file(filename: &str) -> io::Result<u32> {
    let sues: Sues = parse_file(filename)?;
    let mut working_set: HashSet<u32> = sues.keys().cloned().collect();
    let requested_sue_traits: Vec<(&str, u32)> = vec![
        ("children", 3),
        ("cats", 7),
        ("samoyeds", 2),
        ("pomeranians", 3),
        ("akitas", 0),
        ("vizslas", 0),
        ("goldfish", 5),
        ("trees", 3),
        ("cars", 2),
        ("perfumes", 1),
    ];
    for (thing, requested_amount) in requested_sue_traits.iter() {
        let possible = possible_set(&sues, thing, *requested_amount);
        working_set = working_set.intersection(&possible).cloned().collect();
    }
    Ok(*working_set.iter().next().unwrap())
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
