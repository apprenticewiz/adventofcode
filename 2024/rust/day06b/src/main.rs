use std::collections::HashSet;
use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_grid(contents: &str) -> Vec<Vec<char>> {
    let mut grid: Vec<Vec<char>> = vec![];
    for line in contents.lines() {
        let row: Vec<char> = line.chars().collect();
        grid.push(row);
    }
    grid
}

fn find_start(grid: &[Vec<char>]) -> Option<(usize, usize)> {
    let extents = (grid.len(), grid[0].len());
    for (row, row_chars) in grid.iter().enumerate().take(extents.0) {
        for (col, ch) in row_chars.iter().enumerate().take(extents.1) {
            if *ch == '^' {
                return Some((row, col));
            }
        }
    }
    None
}

fn find_obstacles(grid: &[Vec<char>]) -> HashSet<(usize, usize)> {
    let mut obstacles: HashSet<(usize, usize)> = HashSet::new();
    let extents = (grid.len(), grid[0].len());
    for (row, row_chars) in grid.iter().enumerate().take(extents.0) {
        for (col, ch) in row_chars.iter().enumerate().take(extents.1) {
            if *ch == '#' {
                obstacles.insert((row, col));
            }
        }
    }
    obstacles
}

fn turn(dir: &(isize, isize)) -> (isize, isize) {
    match dir {
        (-1, 0) => (0, 1),
        (0, 1) => (1, 0),
        (1, 0) => (0, -1),
        (0, -1) => (-1, 0),
        _ => panic!("unknown direction in turn()"),
    }
}

fn in_bounds(pos: &(isize, isize), extents: &(usize, usize)) -> bool {
    pos.0 >= 0 && pos.0 < extents.0 as isize && pos.1 >= 0 && pos.1 < extents.1 as isize
}

fn walk(
    start_pos: &(usize, usize),
    start_dir: &(isize, isize),
    extents: &(usize, usize),
    obstacles: &HashSet<(usize, usize)>,
    check_cycles: bool,
) -> Vec<((usize, usize), (isize, isize))> {
    let mut path: Vec<((usize, usize), (isize, isize))> = vec![];
    let mut pos = *start_pos;
    let mut dir = *start_dir;
    loop {
        path.push((pos, dir));
        let maybe_pos = (pos.0 as isize + dir.0, pos.1 as isize + dir.1);
        if !in_bounds(&maybe_pos, extents) {
            break;
        } else if check_cycles && path.iter().filter(|&&x| x == *path.last().unwrap()).count() > 1 {
            break;
        }
        let new_pos = (maybe_pos.0 as usize, maybe_pos.1 as usize);
        if obstacles.contains(&new_pos) {
            dir = turn(&dir);
        } else {
            pos = new_pos;
        }
    }
    path
}

fn try_obstacle(
    pos: &(usize, usize),
    dir: &(isize, isize),
    extents: &(usize, usize),
    obstacles: &HashSet<(usize, usize)>,
    new_obstacle: &(usize, usize),
) -> bool {
    let mut new_obstacles = obstacles.clone();
    new_obstacles.insert(*new_obstacle);
    let sim_path = walk(pos, dir, extents, &new_obstacles, true);
    let last_state = sim_path.last().unwrap();
    let count = sim_path.iter().filter(|&&x| x == *last_state).count();
    count > 1
}

fn add_obstacles(
    path: &[((usize, usize), (isize, isize))],
    start_pos: &(usize, usize),
    start_dir: &(isize, isize),
    extents: &(usize, usize),
    obstacles: &HashSet<(usize, usize)>,
) -> HashSet<(usize, usize)> {
    let mut added_obstacles: HashSet<(usize, usize)> = HashSet::new();
    for &current_state in path.iter().skip(1) {
        let new_obstacle = current_state.0;
        if try_obstacle(&start_pos, &start_dir, extents, obstacles, &new_obstacle) {
            added_obstacles.insert(new_obstacle);
        }
    }
    added_obstacles
}

fn process(contents: &str) -> u32 {
    let grid = build_grid(contents);
    let extents = (grid.len(), grid[0].len());
    let start_pos = match find_start(&grid) {
        Some(start) => start,
        None => panic!("start position not found in data!"),
    };
    let start_dir = (-1, 0);
    let obstacles = find_obstacles(&grid);
    let path = walk(&start_pos, &start_dir, &extents, &obstacles, false);
    add_obstacles(&path, &start_pos, &start_dir, &extents, &obstacles).len() as u32
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
