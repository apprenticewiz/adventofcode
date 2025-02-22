use std::collections::{HashMap, HashSet, VecDeque};
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

fn build_grid(contents: &str) -> Vec<Vec<char>> {
    let mut grid: Vec<Vec<char>> = vec![];
    for line in contents.lines() {
        grid.push(line.chars().collect());
    }
    grid
}

fn neighbors(grid: &[Vec<char>], pos: &Position) -> Vec<Position> {
    let mut neighbor_cells: Vec<Position> = vec![];
    let num_rows = grid.len();
    let num_cols = grid[0].len();
    let directions = vec![(-1, 0), (1, 0), (0, -1), (0, 1)];
    let (row, col) = *pos;
    for (dr, dc) in &directions {
        let new_pos = (row as isize + dr, col as isize + dc);
        if let (Ok(new_row), Ok(new_col)) = (new_pos.0.try_into(), new_pos.1.try_into()) {
            if new_row < num_rows && new_col < num_cols {
                neighbor_cells.push((new_row, new_col));
            }
        }
    }
    neighbor_cells
}

fn flood_fill(grid: &[Vec<char>], start_pos: &Position, ch: char) -> HashSet<Position> {
    let mut stack: VecDeque<Position> = VecDeque::new();
    let mut visited: HashSet<Position> = HashSet::new();
    stack.push_back(*start_pos);
    while let Some(pos) = stack.pop_back() {
        if !visited.contains(&pos) {
            let (row, col) = pos;
            if grid[row][col] == ch {
                visited.insert(pos);
                for neighbor in &neighbors(grid, &pos) {
                    stack.push_back(*neighbor);
                }
            }
        }
    }
    visited
}

fn find_components(grid: &[Vec<char>]) -> HashMap<char, Vec<HashSet<Position>>> {
    let num_rows = grid.len();
    let num_cols = grid[0].len();
    let mut components: HashMap<char, Vec<HashSet<Position>>> = HashMap::new();
    let mut visited: HashSet<Position> = HashSet::new();
    let mut q: VecDeque<Position> = VecDeque::new();
    for i in 0..num_rows {
        for j in 0..num_cols {
            q.push_back((i, j));
        }
    }
    while let Some(pos) = q.pop_front() {
        if !visited.contains(&pos) {
            let ch = &grid[pos.0][pos.1];
            let component = flood_fill(grid, &pos, *ch);
            components
                .entry(*ch)
                .and_modify(|x| x.push(component.clone()))
                .or_insert(vec![component.clone()]);
            visited = visited.union(&component).cloned().collect();
        }
    }
    components
}

fn calc_area_and_perimeter(grid: &[Vec<char>], component: &HashSet<Position>) -> (u32, u32) {
    let num_rows = grid.len();
    let num_cols = grid[0].len();
    let area = component.len() as u32;
    let mut perimeter = 0;
    for pos in component {
        let mut boundary_count = 0;
        if pos.0 == 0 || pos.0 == (num_rows - 1) {
            boundary_count += 1;
        }
        if pos.1 == 0 || pos.1 == (num_cols - 1) {
            boundary_count += 1;
        }
        for neighbor in &neighbors(grid, pos) {
            if grid[pos.0][pos.1] != grid[neighbor.0][neighbor.1] {
                boundary_count += 1;
            }
        }
        perimeter += boundary_count;
    }
    (area, perimeter)
}

fn compute_prices(
    grid: &[Vec<char>],
    components: &HashMap<char, Vec<HashSet<Position>>>,
) -> HashMap<char, u32> {
    let mut prices: HashMap<char, u32> = HashMap::new();
    for ch in components.keys() {
        for component in components.get(ch).unwrap() {
            let (area, perimeter) = calc_area_and_perimeter(grid, component);
            prices
                .entry(*ch)
                .and_modify(|x| *x += area * perimeter)
                .or_insert(area * perimeter);
        }
    }
    prices
}

fn process(contents: &str) -> u32 {
    let grid = build_grid(contents);
    let components = find_components(&grid);
    let prices = compute_prices(&grid, &components);
    prices.values().sum()
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
