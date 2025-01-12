use std::collections::VecDeque;
use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

type Position = (usize, usize);

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_grid(contents: &str) -> Vec<Vec<u32>> {
    let mut grid: Vec<Vec<u32>> = vec![];
    for row in contents.lines() {
        let mut row_heights: Vec<u32> = vec![];
        for ch in row.chars() {
            if let Some(num) = ch.to_digit(10) {
                row_heights.push(num);
            }
        }
        grid.push(row_heights);
    }
    grid
}

fn find_trailheads(grid: &[Vec<u32>]) -> Vec<Position> {
    let mut trailheads: Vec<Position> = vec![];
    for (row, row_heights) in grid.iter().enumerate() {
        for (col, height) in row_heights.iter().enumerate() {
            if *height == 0 {
                trailheads.push((row, col));
            }
        }
    }
    trailheads
}

fn compute_trail_rating(grid: &[Vec<u32>], trailhead: &Position) -> u32 {
    let dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)];
    let num_rows = grid.len();
    let num_cols = grid[0].len();
    let mut paths: Vec<Vec<Position>> = vec![];
    let mut q: VecDeque<Vec<Position>> = VecDeque::new();
    q.push_back(vec![*trailhead]);
    while let Some(path) = q.pop_front() {
        let pos = path.iter().last().unwrap();
        let height = grid[pos.0][pos.1];
        if height == 9 {
            paths.push(path);
        } else {
            dirs.iter()
                .map(|(dr, dc)| (pos.0 as isize + dr, pos.1 as isize + dc))
                .filter(|(r, c)| {
                    *r >= 0
                        && *r < (num_rows as isize)
                        && *c >= 0
                        && *c < (num_cols as isize)
                        && grid[*r as usize][*c as usize] == height + 1
                })
                .for_each(|(r, c)| {
                    let mut new_path = path.clone();
                    new_path.push((r as usize, c as usize));
                    q.push_back(new_path);
                });
        }
    }
    paths.len() as u32
}

fn process(contents: &str) -> u32 {
    let grid = build_grid(contents);
    let trailheads = find_trailheads(&grid);
    trailheads
        .iter()
        .map(|s| compute_trail_rating(&grid, s))
        .sum::<u32>()
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
