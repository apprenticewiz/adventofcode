use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::process;

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
                antennas.entry(ch).and_modify(|s| { s.insert(pos); }).or_insert(HashSet::from([pos]));
            }
        }
    }
    (antennas, (num_rows, num_cols))
}

fn in_bounds(pos: &Position, grid_size: &(usize, usize)) -> bool {
    pos.0 >= 0 && pos.0 < (grid_size.0 as isize) && pos.1 >= 0 && pos.1 < (grid_size.1 as isize)
}

fn gen_antinodes(freq: char, antennas: &HashMap<char, HashSet<Position>>, grid_size: &(usize, usize)) -> HashSet<Position> {
    let freq_antennas = antennas.get(&freq).unwrap();
    let combos = freq_antennas.into_iter().combinations(2);
    let mut antinodes: HashSet<Position> = HashSet::new();
    combos.for_each(|combo| {
        let dr = combo[0].0 - combo[1].0;
        let dc = combo[0].1 - combo[1].1;
        let p1 = (combo[0].0 + dr, combo[0].1 + dc);
        let p2 = (combo[1].0 - dr, combo[1].1 - dc);
        if in_bounds(&p1, grid_size) {
            antinodes.insert(p1);
        }
        if in_bounds(&p2, grid_size) {
            antinodes.insert(p2);
        }
    });
    antinodes
}

fn process(contents: &str) -> u64 {
    let (antennas, grid_size) = parse_input(contents);
    let mut antinodes: HashSet<Position> = HashSet::new();
    for freq in antennas.keys() {
        antinodes = antinodes.union(&gen_antinodes(*freq, &antennas, &grid_size)).cloned().collect();
    }
    antinodes.len() as u64
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
