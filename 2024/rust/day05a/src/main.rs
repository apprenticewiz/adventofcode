use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn read_input(contents: &str) -> (HashMap<u32, Vec<u32>>, Vec<Vec<u32>>) {
    let mut scanning_rules = true;
    let mut rules = HashMap::new();
    let mut updates = vec![];
    for line in contents.lines() {
        match line {
            "" => scanning_rules = false,
            _ => {
                if scanning_rules {
                    let (first_txt, second_txt) = line.split_once("|").unwrap();
                    let first = first_txt.parse::<u32>().unwrap();
                    let second = second_txt.parse::<u32>().unwrap();
                    match rules.get(&first) {
                        None => rules.insert(first, vec![second]),
                        Some(following) => {
                            let mut new_following = following.clone();
                            new_following.push(second);
                            rules.insert(first, new_following)
                        }
                    };
                } else {
                    let update = line.split(',').map(|s| s.parse::<u32>().unwrap()).collect();
                    updates.push(update);
                }
            }
        }
    }
    (rules, updates)
}

fn is_valid(rules: &HashMap<u32, Vec<u32>>, update: &[u32]) -> bool {
    let mut valid = true;
    for i in 0..update.len() {
        let x = &update[i];
        let xs = &update[(i + 1)..];
        for y in xs.iter() {
            valid &= rules.contains_key(x) && rules[x].contains(y);
        }
    }
    valid
}

fn process_updates(rules: &HashMap<u32, Vec<u32>>, updates: &[Vec<u32>]) -> u32 {
    let mut result = 0;
    for update in updates.iter() {
        if is_valid(rules, update) {
            let mid_idx = update.len() / 2;
            let mid_value = &update[mid_idx];
            result += mid_value;
        }
    }
    result
}

fn process(contents: &str) -> u32 {
    let (rules, updates) = read_input(contents);
    process_updates(&rules, &updates)
}

fn calc_runtime(start_time: &SystemTime) -> String {
    let mut elapsed = start_time.elapsed().unwrap().as_micros() as f64;
    let mut units = "μs";
    if elapsed > 1000.0 {
        elapsed /= 1000.0;
        units = "ms";
    }
    if elapsed > 1000.0 {
        elapsed /= 1000.0;
        units = "s";
    }
    format!("(elapsed time: {elapsed} {units})")
}

fn main() {
    let start_time = SystemTime::now();
    if env::args().count() < 2 {
        usage();
    }
    let filename = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(filename).expect("read of input file failed");
    let result = process(&contents);
    let duration = calc_runtime(&start_time);
    println!("result = {result}  {duration}");
}
