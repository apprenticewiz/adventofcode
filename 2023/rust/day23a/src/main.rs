use std::collections::VecDeque;
use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Position(i32, i32);

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_grid(contents: &str) -> Vec<Vec<char>> {
    let mut grid: Vec<Vec<char>> = vec![];
    for line in contents.lines() {
        grid.push(line.chars().collect::<Vec<char>>());
    }
    grid
}

fn in_bounds(pos: Position, extents: Position) -> bool {
    pos.0 >= 0 && pos.0 < extents.0 && pos.1 >= 0 && pos.1 < extents.1
}

fn valid_step(object: char) -> bool {
    object != '#'
}

fn walk_grid(grid: &Vec<Vec<char>>, start: &Position, end: &Position) -> u32 {
    let mut q: VecDeque<Vec<Position>> = VecDeque::new();
    let mut paths: Vec<Vec<Position>> = vec![];
    let num_rows = grid.len() as i32;
    let num_cols = grid[0].len() as i32;
    let extents: Position = Position(num_rows, num_cols);
    q.push_back(vec![*start]);
    while !q.is_empty() {
        let mut path = q.pop_front().unwrap().clone();
        let path_end = *path.iter().last().unwrap();
        if path_end == *end {
            paths.push(path);
        } else {
            let object = &grid[path_end.0 as usize][path_end.1 as usize];
            match object {
                '^' => {
                    let neighbor = Position(path_end.0 - 1, path_end.1);
                    if !path.contains(&neighbor)
                        && in_bounds(neighbor, extents)
                        && valid_step(grid[neighbor.0 as usize][neighbor.1 as usize])
                    {
                        path.push(neighbor);
                        q.push_back(path.clone());
                    }
                }
                'v' => {
                    let neighbor = Position(path_end.0 + 1, path_end.1);
                    if !path.contains(&neighbor)
                        && in_bounds(neighbor, extents)
                        && valid_step(grid[neighbor.0 as usize][neighbor.1 as usize])
                    {
                        path.push(neighbor);
                        q.push_back(path.clone());
                    }
                }
                '<' => {
                    let neighbor = Position(path_end.0, path_end.1 - 1);
                    if !path.contains(&neighbor)
                        && in_bounds(neighbor, extents)
                        && valid_step(grid[neighbor.0 as usize][neighbor.1 as usize])
                    {
                        path.push(neighbor);
                        q.push_back(path.clone());
                    }
                }
                '>' => {
                    let neighbor = Position(path_end.0, path_end.1 + 1);
                    if !path.contains(&neighbor)
                        && in_bounds(neighbor, extents)
                        && valid_step(grid[neighbor.0 as usize][neighbor.1 as usize])
                    {
                        path.push(neighbor);
                        q.push_back(path.clone());
                    }
                }
                '.' => {
                    let mut neighbor = Position(path_end.0 - 1, path_end.1);
                    if !path.contains(&neighbor)
                        && in_bounds(neighbor, extents)
                        && valid_step(grid[neighbor.0 as usize][neighbor.1 as usize])
                    {
                        let mut path_clone = path.clone();
                        path_clone.push(neighbor);
                        q.push_back(path_clone);
                    }
                    neighbor = Position(path_end.0 + 1, path_end.1);
                    if !path.contains(&neighbor)
                        && in_bounds(neighbor, extents)
                        && valid_step(grid[neighbor.0 as usize][neighbor.1 as usize])
                    {
                        let mut path_clone = path.clone();
                        path_clone.push(neighbor);
                        q.push_back(path_clone);
                    }
                    neighbor = Position(path_end.0, path_end.1 - 1);
                    if !path.contains(&neighbor)
                        && in_bounds(neighbor, extents)
                        && valid_step(grid[neighbor.0 as usize][neighbor.1 as usize])
                    {
                        let mut path_clone = path.clone();
                        path_clone.push(neighbor);
                        q.push_back(path_clone);
                    }
                    neighbor = Position(path_end.0, path_end.1 + 1);
                    if !path.contains(&neighbor)
                        && in_bounds(neighbor, extents)
                        && valid_step(grid[neighbor.0 as usize][neighbor.1 as usize])
                    {
                        let mut path_clone = path.clone();
                        path_clone.push(neighbor);
                        q.push_back(path_clone);
                    }
                }
                _ => panic!("BUG: unexpected character encountered in grid!"),
            }
        }
    }
    paths.iter().map(|x| x.len()).max().unwrap() as u32 - 1
}

fn process(contents: &str) -> u32 {
    let grid = build_grid(contents);
    let start = Position(0, 1);
    let num_rows = grid.len() as i32;
    let num_cols = grid[0].len() as i32;
    let end = Position(num_rows - 1, num_cols - 2);
    walk_grid(&grid, &start, &end)
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
