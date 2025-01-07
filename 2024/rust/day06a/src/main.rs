use std::collections::HashSet;
use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

type Position = (usize, usize);

struct Lab {
    start: Position,
    direction: usize,
    grid: Vec<Vec<char>>,
    num_rows: usize,
    num_cols: usize,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn parse(contents: &str) -> Lab {
    let mut grid: Vec<Vec<char>> = vec![];
    for line in contents.lines() {
        grid.push(line.chars().collect::<Vec<char>>());
    }
    let num_rows: usize = grid.len();
    let num_cols: usize = grid[0].len();
    let mut start: Position = (0, 0);
    for (row, row_chars) in grid.iter_mut().enumerate() {
        for (col, ch) in row_chars.iter_mut().enumerate() {
            if *ch == '^' {
                start = (row, col);
                *ch = '.';
            }
        }
    }
    Lab {
        start,
        direction: 0,
        grid,
        num_rows,
        num_cols,
    }
}

fn guard_positions(lab: &Lab) -> Vec<(Position, usize)> {
    let directions = [(-1, 0), (0, 1), (1, 0), (0, -1)];
    let mut positions: Vec<(Position, usize)> = vec![];
    let mut pos = lab.start;
    let mut dir = lab.direction;
    loop {
        positions.push((pos, dir));
        let next_pos = (
            pos.0 as isize + directions[dir].0,
            pos.1 as isize + directions[dir].1,
        );
        if next_pos.0 < 0
            || next_pos.0 >= lab.num_rows as isize
            || next_pos.1 < 0
            || next_pos.1 >= lab.num_cols as isize
        {
            return positions;
        }
        if lab.grid[next_pos.0 as usize][next_pos.1 as usize] == '#' {
            dir = (dir + 1) % 4;
        }
        pos = (
            (pos.0 as isize + directions[dir].0) as usize,
            (pos.1 as isize + directions[dir].1) as usize,
        );
    }
}

fn process(contents: &str) -> u64 {
    let lab = parse(contents);
    let positions = guard_positions(&lab);
    let unique_positions: HashSet<Position> = positions
        .into_iter()
        .map(|p| p.0)
        .collect::<HashSet<Position>>();
    unique_positions.len() as u64
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
