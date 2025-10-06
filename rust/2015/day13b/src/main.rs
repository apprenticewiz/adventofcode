use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use itertools::Itertools;
use regex::Regex;

type HappinessTable = HashMap<(String, String), i32>;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_file(filename: &str) -> io::Result<HappinessTable> {
    let mut table: HappinessTable = HashMap::new();
    let re = Regex::new(r"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).")
        .unwrap();
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    for line_result in reader.lines() {
        let line = line_result?;
        if let Some(caps) = re.captures(&line) {
            let p1 = String::from(caps.get(1).unwrap().as_str());
            let p2 = String::from(caps.get(4).unwrap().as_str());
            let amt = match caps.get(2).unwrap().as_str() {
                "gain" => caps.get(3).unwrap().as_str().parse::<i32>().unwrap(),
                "lose" => -(caps.get(3).unwrap().as_str().parse::<i32>().unwrap()),
                _ => unreachable!(),
            };
            table.insert((p1, p2), amt);
        }
    }
    Ok(table)
}

fn compute_score(table: &HappinessTable, people: &[String]) -> i32 {
    let n = people.len();
    let mut total = 0;
    for i in 0..n {
        let p = &people[i];
        let l = &people[(i + n - 1) % n];
        let r = &people[(i + 1) % n];
        total += table.get(&(p.clone(), l.clone())).unwrap_or(&0);
        total += table.get(&(p.clone(), r.clone())).unwrap_or(&0);
    }
    total
}

fn inject_self(table: &mut HappinessTable) {
    let mut people_set: HashSet<String> = HashSet::new();
    table.keys().for_each(|(p1, p2)| {
        people_set.insert(p1.clone());
        people_set.insert(p2.clone());
    });
    let people: Vec<String> = people_set.into_iter().collect();
    for person in people.iter() {
        table.insert((String::from(person), String::from("Self")), 0);
        table.insert((String::from("Self"), String::from(person)), 0);
    }
}

fn process_file(filename: &str) -> io::Result<i32> {
    let mut table = parse_file(filename)?;
    inject_self(&mut table);
    let mut people_set: HashSet<String> = HashSet::new();
    table.keys().for_each(|(p1, p2)| {
        people_set.insert(p1.clone());
        people_set.insert(p2.clone());
    });
    let mut people: Vec<String> = people_set.into_iter().collect();
    people.sort();
    let (fixed, rest) = people.split_first().unwrap();
    let result = rest
        .iter()
        .permutations(rest.len())
        .map(|perm| {
            let mut arrangement = vec![fixed.clone()];
            arrangement.extend(perm.into_iter().cloned());
            compute_score(&table, &arrangement)
        })
        .max()
        .unwrap_or(0);
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
