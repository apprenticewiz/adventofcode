use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use itertools::Itertools;
use regex::Regex;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_file(filename: &str) -> io::Result<Vec<(String, String, u32)>> {
    let line_re = Regex::new(r"([A-Za-z]+) to ([A-Za-z]+) = (\d+)").unwrap();
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut input_data: Vec<(String, String, u32)> = vec![];
    for line_result in reader.lines() {
        let line = line_result?;
        if let Some(caps) = line_re.captures(&line) {
            let from = String::from(&caps[1]);
            let to = String::from(&caps[2]);
            let dist = caps[3].parse::<u32>().unwrap();
            input_data.push((from, to, dist));
        }
    }
    Ok(input_data)
}

fn build_map(edges: &[(String, String, u32)]) -> HashMap<(String, String), u32> {
    let mut distances: HashMap<(String, String), u32> = HashMap::new();
    edges.iter().for_each(|(c1, c2, d)| {
        distances.insert((c1.clone(), c2.clone()), *d);
        distances.insert((c2.clone(), c1.clone()), *d);
    });
    distances
}

fn path_len(distances: &HashMap<(String, String), u32>, path: &[&String]) -> u32 {
    path.windows(2)
        .map(|cs| {
            let c1 = cs[0];
            let c2 = cs[1];
            distances.get(&(c1.clone(), c2.clone())).unwrap()
        })
        .sum()
}

fn shortest_route(cities: &[String], distances: &HashMap<(String, String), u32>) -> u32 {
    cities
        .iter()
        .permutations(cities.len())
        .map(|path| path_len(distances, &path))
        .min()
        .unwrap()
}

fn process_file(filename: &str) -> io::Result<u32> {
    let edges = parse_file(filename)?;
    let distances = build_map(&edges);
    let mut tmp_set: HashSet<String> = HashSet::new();
    distances.keys().for_each(|(c1, c2)| {
        tmp_set.insert(c1.clone());
        tmp_set.insert(c2.clone());
    });
    let cities: Vec<String> = tmp_set.into_iter().collect();
    let result = shortest_route(&cities, &distances);
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
