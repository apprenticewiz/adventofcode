use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

type Grid = Vec<Vec<u32>>;

const NUM_STEPS: u32 = 100;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_file(filename: &str) -> io::Result<Grid> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut grid: Grid = vec![];
    for line_result in reader.lines() {
        let line = line_result?;
        let row: Vec<u32> = line
            .chars()
            .map(|x| match x {
                '#' => 1,
                '.' => 0,
                _ => panic!("malformed input: characters must be # or ."),
            })
            .collect();
        grid.push(row);
    }
    Ok(grid)
}

fn count_neighbors(grid: &Grid, row: usize, col: usize) -> u32 {
    let deltas: [(isize, isize); 8] = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];
    let num_rows = grid.len();
    let num_cols = grid[0].len();
    deltas
        .iter()
        .map(|(i, j)| (row as isize + i, col as isize + j))
        .filter(|(r, c)| *r >= 0 && *r < num_rows as isize && *c >= 0 && *c < num_cols as isize)
        .map(|(r, c)| grid[r as usize][c as usize])
        .sum()
}

fn process_file(filename: &str) -> io::Result<u32> {
    let mut grid = parse_file(filename)?;
    let num_rows = grid.len();
    let num_cols = grid[0].len();
    grid[0][0] = 1;
    grid[0][num_cols - 1] = 1;
    grid[num_rows - 1][0] = 1;
    grid[num_rows - 1][num_cols - 1] = 1;
    for _ in 0..NUM_STEPS {
        let mut next_grid: Grid = vec![];
        for row in 0..num_rows {
            let mut next_row: Vec<u32> = vec![];
            for col in 0..num_cols {
                let neighbors_on = count_neighbors(&grid, row, col);
                match &grid[row][col] {
                    1 => {
                        if (2..=3).contains(&neighbors_on) {
                            next_row.push(1);
                        } else {
                            next_row.push(0);
                        }
                    }
                    0 => {
                        if neighbors_on == 3 {
                            next_row.push(1);
                        } else {
                            next_row.push(0);
                        }
                    }
                    _ => panic!("unexpected value in grid!"),
                }
            }
            next_grid.push(next_row);
        }
        grid = next_grid;
        grid[0][0] = 1;
        grid[0][num_cols - 1] = 1;
        grid[num_rows - 1][0] = 1;
        grid[num_rows - 1][num_cols - 1] = 1;
    }
    let count = grid.iter().map(|x| x.iter().sum::<u32>()).sum();
    Ok(count)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let filename = &args[1];
    match process_file(filename) {
        Ok(result) => println!("result = {result}"),
        Err(e) => {
            eprintln!("error while processing file `{filename}': {e}");
            process::exit(1);
        }
    }
}
