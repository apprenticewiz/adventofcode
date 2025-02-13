use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

type Position = (isize, isize);

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn parse_input(contents: &str) -> (HashMap<char, HashSet<Position>>, (usize, usize)) {
    let mut antennas: HashMap<char, HashSet<Position>> = HashMap::new();
    let num_rows = contents.lines().count();
    let num_cols = contents.lines().next().unwrap().len();
    for (row, row_chars) in contents.lines().enumerate() {
        for (col, ch) in row_chars.chars().enumerate() {
            if ch != '.' {
                let pos = (row as isize, col as isize);
                antennas
                    .entry(ch)
                    .and_modify(|s| {
                        s.insert(pos);
                    })
                    .or_insert(HashSet::from([pos]));
            }
        }
    }
    (antennas, (num_rows, num_cols))
}

fn in_bounds(pos: &Position, grid_size: &(usize, usize)) -> bool {
    pos.0 >= 0 && pos.0 < (grid_size.0 as isize) && pos.1 >= 0 && pos.1 < (grid_size.1 as isize)
}

fn gen_antinodes(
    freq: char,
    antennas: &HashMap<char, HashSet<Position>>,
    grid_size: &(usize, usize),
) -> HashSet<Position> {
    let freq_antennas = antennas.get(&freq).unwrap();
    let combos = freq_antennas.iter().combinations(2);
    let mut antinodes: HashSet<Position> = HashSet::new();
    combos.for_each(|combo| {
        let dr = combo[0].0 - combo[1].0;
        let dc = combo[0].1 - combo[1].1;
        let mut pos = *combo[0];
        antinodes.insert(pos);
        loop {
            pos = (pos.0 - dr, pos.1 - dc);
            if in_bounds(&pos, grid_size) {
                antinodes.insert(pos);
            } else {
                break;
            }
        }
        let mut pos = *combo[0];
        loop {
            pos = (pos.0 + dr, pos.1 + dc);
            if in_bounds(&pos, grid_size) {
                antinodes.insert(pos);
            } else {
                break;
            }
        }
    });
    antinodes
}

fn process(contents: &str) -> u64 {
    let (antennas, grid_size) = parse_input(contents);
    let mut antinodes: HashSet<Position> = HashSet::new();
    for freq in antennas.keys() {
        antinodes = antinodes
            .union(&gen_antinodes(*freq, &antennas, &grid_size))
            .cloned()
            .collect();
    }
    antinodes.len() as u64
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
