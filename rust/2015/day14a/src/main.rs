use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use regex::Regex;

const TIME: u32 = 2503;

struct Reindeer {
    velocity: u32,
    flying_time: u32,
    rest_time: u32,
}

type ReindeerTable = HashMap<String, Reindeer>;

enum ReindeerAction {
    Flying(u32),
    Resting(u32),
}

struct ReindeerState {
    action: ReindeerAction,
    position: u32,
}

type StateTable = HashMap<String, ReindeerState>;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_file(filename: &str) -> io::Result<ReindeerTable> {
    let mut table: ReindeerTable = HashMap::new();
    let re = Regex::new(
        r"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.",
    )
    .unwrap();
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    for line_result in reader.lines() {
        let line = line_result?;
        if let Some(caps) = re.captures(&line) {
            let name = String::from(caps.get(1).unwrap().as_str());
            let reindeer = Reindeer {
                velocity: caps.get(2).unwrap().as_str().parse::<u32>().unwrap_or(0),
                flying_time: caps.get(3).unwrap().as_str().parse::<u32>().unwrap_or(0),
                rest_time: caps.get(4).unwrap().as_str().parse::<u32>().unwrap_or(0),
            };
            table.insert(name, reindeer);
        }
    }
    Ok(table)
}

fn init_race(table: &ReindeerTable) -> StateTable {
    let mut state: StateTable = HashMap::new();
    table.iter().for_each(|(name, data)| {
        state.insert(
            name.clone(),
            ReindeerState {
                action: ReindeerAction::Flying(data.flying_time - 1),
                position: 0,
            },
        );
    });
    state
}

fn process_file(filename: &str) -> io::Result<u32> {
    let table = parse_file(filename)?;
    let mut state = init_race(&table);
    for _ in 0..TIME {
        for (name, curr_state) in state.iter_mut() {
            let reindeer = table.get(name).unwrap();
            match curr_state.action {
                ReindeerAction::Flying(n) => {
                    curr_state.position += reindeer.velocity;
                    if n > 0 {
                        curr_state.action = ReindeerAction::Flying(n - 1);
                    } else {
                        curr_state.action = ReindeerAction::Resting(reindeer.rest_time - 1);
                    }
                }
                ReindeerAction::Resting(n) => {
                    if n > 0 {
                        curr_state.action = ReindeerAction::Resting(n - 1);
                    } else {
                        curr_state.action = ReindeerAction::Flying(reindeer.flying_time - 1);
                    }
                }
            }
        }
    }
    Ok(state.values().max_by_key(|v| v.position).unwrap().position)
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
