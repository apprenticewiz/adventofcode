use std::collections::HashMap;
use std::env;
use std::fs;

type Graph = HashMap<String, Vec<String>>;
type MemoKey = (String, bool, bool);
type Memo = HashMap<MemoKey, i64>;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    std::process::exit(1);
}

fn parse_line(line: &str) -> Option<(String, Vec<String>)> {
    let (key, rest) = line.split_once(':')?;
    let neighbors: Vec<String> = rest.split_whitespace().map(String::from).collect();
    Some((key.to_string(), neighbors))
}

fn count_paths(graph: &Graph, node: &str, has_dac: bool, has_fft: bool, memo: &mut Memo) -> i64 {
    if node == "out" {
        return if has_dac && has_fft { 1 } else { 0 };
    }
    let has_dac = has_dac || node == "dac";
    let has_fft = has_fft || node == "fft";
    let key = (node.to_string(), has_dac, has_fft);
    if let Some(&cached) = memo.get(&key) {
        return cached;
    }
    let neighbors = match graph.get(node) {
        Some(ns) => ns.clone(),
        None => Vec::new(),
    };
    let total: i64 = neighbors
        .iter()
        .map(|n| count_paths(graph, n, has_dac, has_fft, memo))
        .sum();
    memo.insert(key, total);
    total
}

fn process(content: &str) -> i64 {
    let graph: Graph = content
        .lines()
        .filter(|line| !line.is_empty())
        .filter_map(parse_line)
        .collect();
    let mut memo = Memo::new();
    count_paths(&graph, "svr", false, false, &mut memo)
}

fn display_duration(duration: u128) {
    print!("elapsed time: ");
    let duration_f = duration as f64;
    if duration_f < 1_000.0 {
        println!("{} ns", duration_f);
    } else if duration_f < 1_000_000.0 {
        println!("{} μs", duration_f / 1_000.0);
    } else if duration_f < 1_000_000_000.0 {
        println!("{} ms", duration_f / 1_000_000.0);
    } else {
        println!("{} s", duration_f / 1_000_000_000.0);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let filename = &args[1];
    let start = std::time::Instant::now();
    match fs::read_to_string(filename) {
        Ok(content) => {
            let result = process(&content);
            let duration = start.elapsed().as_nanos();
            println!("result = {result}");
            display_duration(duration);
        }
        Err(e) => {
            eprintln!("error while processing file `{filename}': {e}");
            std::process::exit(1);
        }
    }
}
