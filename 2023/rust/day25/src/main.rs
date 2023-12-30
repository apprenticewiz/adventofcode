use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_graph(contents: &str) -> BTreeMap<String, Vec<String>> {
    let mut graph: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for line in contents.lines() {
        if let Some((source_part, targets_part)) = line.split_once(": ") {
            let source = source_part.to_string();
            for target in targets_part.split(' ') {
                graph
                    .entry(source.clone())
                    .and_modify(|e| {
                        e.push(target.to_string());
                    })
                    .or_insert(vec![target.to_string()]);
                graph
                    .entry(target.to_string())
                    .and_modify(|e| {
                        e.push(source.clone());
                    })
                    .or_insert(vec![source.clone()]);
            }
        }
    }
    graph
}

fn scan_components(graph: &BTreeMap<String, Vec<String>>) -> u32 {
    let mut first_group: u32 = 1;
    let mut second_group: u32 = 0;
    let mut components = graph.keys();
    let first_component = components.next().unwrap();
    for component in components {
        let mut connections: u32 = 0;
        let mut tested: BTreeSet<String> = BTreeSet::from([first_component.clone()]);
        for starting_component in graph.get(first_component).unwrap() {
            if *starting_component == *component {
                connections += 1;
                continue;
            }
            let mut seen: BTreeSet<String> = BTreeSet::new();
            let mut q: VecDeque<(String, Vec<String>)> = VecDeque::new();
            let mut found = false;
            q.push_back((starting_component.clone(), vec![starting_component.clone()]));
            while !q.is_empty() && !found && connections < 4 {
                let (other_component, path) = q.pop_front().unwrap();
                for c in graph.get(&other_component).unwrap() {
                    if *component == *c {
                        connections += 1;
                        for p in path.iter() {
                            tested.insert(p.clone());
                        }
                        found = true;
                        break;
                    }
                    else if !seen.contains(c) && !path.contains(&c) && !tested.contains(c) {
                        let mut new_path = path.clone();
                        new_path.push(c.clone());
                        q.push_back((c.clone(), new_path));
                        seen.insert(c.clone());
                    }
                }
            }
        }
        if connections >= 4 {
            first_group += 1;
        } else {
            second_group += 1;
        }
    }
    first_group * second_group
}

fn process(contents: &str) -> u32 {
    let graph = build_graph(contents);
    scan_components(&graph)
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
