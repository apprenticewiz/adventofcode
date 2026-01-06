use std::collections::HashSet;
use std::env;
use std::fs;
use std::io;
use std::process;
use std::time::Instant;

type Position = (i64, i64);

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn in_range(bounds: &(Position, Position), pos: &Position) -> bool {
    let ((lbr, lbc), (ubr, ubc)) = bounds;
    let (pr, pc) = pos;
    pr >= lbr && pr <= ubr && pc >= lbc && pc <= ubc
}

fn simulate(
    bounds: &(Position, Position),
    splitters: &HashSet<Position>,
    start: Position,
) -> HashSet<Position> {
    let mut stack = Vec::new();
    let mut visited = HashSet::new();
    let mut split_locs = HashSet::new();
    stack.push(start);
    while let Some((r, c)) = stack.pop() {
        let pos = (r, c);
        if !in_range(bounds, &pos) {
            continue;
        }
        if !visited.insert(pos) {
            continue;
        }
        if splitters.contains(&pos) {
            split_locs.insert(pos);
            stack.push((r, c - 1));
            stack.push((r, c + 1));
        } else {
            stack.push((r + 1, c));
        }
    }
    split_locs
}

fn process_file(filename: &str) -> io::Result<i64> {
    let contents = fs::read_to_string(filename)?;
    let rows: i64 = contents.lines().count() as i64;
    let cols: i64 = contents.lines().next().unwrap().chars().count() as i64;
    let bounds = ((0, 0), (rows, cols));
    let mut start: Option<Position> = None;
    let mut splitters: HashSet<Position> = HashSet::new();
    for (row, line) in contents.lines().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            match ch {
                'S' => start = Some((row as i64, col as i64)),
                '^' => {
                    let _ = splitters.insert((row as i64, col as i64));
                }
                _ => {}
            }
        }
    }
    if start.is_none() {
        panic!("no start position found in input!");
    }
    let split_locs = simulate(&bounds, &splitters, start.unwrap());
    Ok(split_locs.len() as i64)
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
    let start = Instant::now();
    match process_file(filename) {
        Ok(result) => {
            let duration = start.elapsed().as_nanos();
            println!("result = {result}");
            display_duration(duration);
        }
        Err(e) => {
            eprintln!("error while processing file `{filename}': {e}");
            process::exit(1);
        }
    }
}
