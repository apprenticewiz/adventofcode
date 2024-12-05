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

fn scan_xs(grid: &[Vec<char>]) -> Vec<(u32, u32)> {
    let max_row = grid.len();
    let max_col = grid[0].len();
    let mut x_cells = Vec::new();
    for (row, chars) in grid.iter().enumerate().take(max_row) {
        for (col, ch) in chars.iter().enumerate().take(max_col) {
            if *ch == 'X' {
                x_cells.push((row as u32, col as u32));
            }
        }
    }
    x_cells
}

fn find_xmas(grid: &[Vec<char>], x_cells: &[(u32, u32)]) -> u32 {
    let mut count = 0;
    let max_row = grid.len() as i32;
    let max_col = grid[0].len() as i32;
    let deltas = [
        [(0, 0), (-1, -1), (-2, -2), (-3, -3)],
        [(0, 0), (-1, 0), (-2, 0), (-3, 0)],
        [(0, 0), (-1, 1), (-2, 2), (-3, 3)],
        [(0, 0), (0, -1), (0, -2), (0, -3)],
        [(0, 0), (0, 1), (0, 2), (0, 3)],
        [(0, 0), (1, -1), (2, -2), (3, -3)],
        [(0, 0), (1, 0), (2, 0), (3, 0)],
        [(0, 0), (1, 1), (2, 2), (3, 3)],
    ];
    for x_cell in x_cells {
        let sx_cell = (x_cell.0 as i32, x_cell.1 as i32);
        for delta in deltas {
            if delta
                .iter()
                .map(|(r, c)| (sx_cell.0 + r, sx_cell.1 + c))
                .all(|(r, c)| r >= 0 && r < max_row && c >= 0 && c < max_col)
            {
                let cells_to_scan: Vec<(i32, i32)> = delta
                    .iter()
                    .map(|(r, c)| (sx_cell.0 + r, sx_cell.1 + c))
                    .collect();
                let chars: String = cells_to_scan
                    .iter()
                    .map(|(r, c)| grid[*r as usize][*c as usize])
                    .collect();
                if chars == "XMAS" {
                    count += 1;
                }
            }
        }
    }
    count
}

fn process(contents: &str) -> u32 {
    let grid = build_grid(contents);
    let x_cells = scan_xs(&grid);
    find_xmas(&grid, &x_cells)
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
