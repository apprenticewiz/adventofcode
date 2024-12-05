use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_grid(contents: &str) -> Vec<Vec<char>> {
    let mut grid = Vec::new();
    for line in contents.lines() {
        let row: Vec<char> = line.chars().collect();
        grid.push(row);
    }
    grid
}

fn scan_as(grid: &[Vec<char>]) -> Vec<(u32, u32)> {
    let max_row = grid.len();
    let max_col = grid[0].len();
    let mut a_cells = Vec::new();
    for (row, chars) in grid.iter().enumerate().take(max_row) {
        for (col, ch) in chars.iter().enumerate().take(max_col) {
            if *ch == 'A' {
                a_cells.push((row as u32, col as u32));
            }
        }
    }
    a_cells
}

fn find_xmas(grid: &[Vec<char>], a_cells: &[(u32, u32)]) -> u32 {
    let mut count = 0;
    let max_row = grid.len() as i32;
    let max_col = grid[0].len() as i32;
    let left_delta = [(-1, -1), (0, 0), (1, 1)];
    let right_delta = [(-1, 1), (0, 0), (1, -1)];
    for a_cell in a_cells {
        let sa_cell = (a_cell.0 as i32, a_cell.1 as i32);
        if left_delta
            .iter()
            .map(|(r, c)| (sa_cell.0 + r, sa_cell.1 + c))
            .all(|(r, c)| r >= 0 && r < max_row && c >= 0 && c < max_col)
            && right_delta
                .iter()
                .map(|(r, c)| (sa_cell.0 + r, sa_cell.1 + c))
                .all(|(r, c)| r >= 0 && r < max_row && c >= 0 && c < max_col)
        {
            let left_cells_to_scan: Vec<(i32, i32)> = left_delta
                .iter()
                .map(|(r, c)| (sa_cell.0 + r, sa_cell.1 + c))
                .collect();
            let right_cells_to_scan: Vec<(i32, i32)> = right_delta
                .iter()
                .map(|(r, c)| (sa_cell.0 + r, sa_cell.1 + c))
                .collect();
            let left_chars: String = left_cells_to_scan
                .iter()
                .map(|(r, c)| grid[*r as usize][*c as usize])
                .collect();
            let right_chars: String = right_cells_to_scan
                .iter()
                .map(|(r, c)| grid[*r as usize][*c as usize])
                .collect();
            if (left_chars == "MAS" || left_chars == "SAM")
                && (right_chars == "MAS" || right_chars == "SAM")
            {
                count += 1;
            }
        }
    }
    count
}

fn process(contents: &str) -> u32 {
    let grid = build_grid(contents);
    let a_cells = scan_as(&grid);
    find_xmas(&grid, &a_cells)
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
