use std::env;
use std::fs;
use std::io;
use std::process;
use std::time::Instant;

type Lights = u64;
type Button = u64;

#[derive(Debug, Clone)]
struct Machine {
    goal: Lights,
    buttons: Vec<Button>,
}

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_lights(s: &str) -> Option<Lights> {
    let s = s.strip_prefix('[')?.strip_suffix(']')?;
    let mut lights = 0u64;
    for (i, ch) in s.chars().enumerate() {
        if ch == '#' {
            lights |= 1 << i;
        }
    }
    Some(lights)
}

fn parse_button(s: &str) -> Option<Button> {
    let s = s.strip_prefix('(')?.strip_suffix(')')?;
    let mut button = 0u64;
    for num_str in s.split(',') {
        let i: u32 = num_str.parse().ok()?;
        button |= 1 << i;
    }
    Some(button)
}

fn parse_joltage(s: &str) -> Option<Vec<i64>> {
    let s = s.strip_prefix('{')?.strip_suffix('}')?;
    s.split(',')
        .map(|num_str| num_str.parse::<i64>().ok())
        .collect()
}

fn parse_line(line: &str) -> Option<Machine> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.is_empty() {
        return None;
    }
    let goal = parse_lights(parts[0])?;
    let mut buttons = Vec::new();
    let mut i = 1;
    while i < parts.len() && parts[i].starts_with('(') {
        buttons.push(parse_button(parts[i])?);
        i += 1;
    }
    if i < parts.len() {
        parse_joltage(parts[i])?;
    }
    Some(Machine { goal, buttons })
}

fn bit_indices(n: u64) -> Vec<usize> {
    (0..64).filter(|&i| (n & (1 << i)) != 0).collect()
}

fn buttons_for_light(goal: Lights, buttons: &[Button]) -> Vec<(u64, u64)> {
    let goal_bits = bit_indices(goal);
    let button_bits: Vec<Vec<usize>> = buttons.iter().map(|&b| bit_indices(b)).collect();
    let max_light = *goal_bits
        .iter()
        .chain(button_bits.iter().flat_map(|v| v.iter()))
        .max()
        .unwrap_or(&0);
    let mut rows = Vec::new();
    for i in 0..=max_light {
        let rhs_bit = if (goal & (1 << i)) != 0 { 1 } else { 0 };
        let mut row_mask = 0u64;
        for (j, button) in buttons.iter().enumerate() {
            if (button & (1 << i)) != 0 {
                row_mask |= 1 << j;
            }
        }
        rows.push((row_mask, rhs_bit));
    }
    rows
}

fn find_pivot(col: usize, start_row: usize, rows: &[(u64, u64)]) -> Option<usize> {
    (start_row..rows.len()).find(|&r| (rows[r].0 & (1 << col)) != 0)
}

fn eliminate(mut rows: Vec<(u64, u64)>, n: usize) -> (Vec<(u64, u64)>, Vec<usize>, Vec<usize>) {
    let mut pivots = Vec::new();
    let mut frees = Vec::new();
    let mut row = 0;
    for col in 0..n {
        if let Some(pivot_row) = find_pivot(col, row, &rows) {
            if row != pivot_row {
                rows.swap(row, pivot_row);
            }
            let (p_mask, p_rhs) = rows[row];
            for (r, current_row) in rows.iter_mut().enumerate() {
                if r != row && (current_row.0 & (1 << col)) != 0 {
                    current_row.0 ^= p_mask;
                    current_row.1 ^= p_rhs;
                }
            }
            pivots.push(col);
            row += 1;
        } else {
            frees.push(col);
        }
    }
    (rows, pivots, frees)
}

fn back_substitute(rows: &[(u64, u64)], pivots: &[usize], _frees: &[usize]) -> u64 {
    let mut x = 0u64;
    for &pivot_col in pivots {
        if let Some(&rhs) = rows
            .iter()
            .find(|(mask, _)| (mask & (1 << pivot_col)) != 0)
            .map(|(_, rhs)| rhs)
            && rhs == 1
        {
            x |= 1 << pivot_col;
        }
    }
    x
}

fn build_nullspace_basis(rows: &[(u64, u64)], pivots: &[usize], frees: &[usize]) -> Vec<u64> {
    let mut basis = Vec::new();
    for &free_col in frees {
        let mut vec = 1u64 << free_col;
        for &pivot_col in pivots {
            if let Some(&mask) = rows
                .iter()
                .find(|(m, _)| (m & (1 << pivot_col)) != 0)
                .map(|(m, _)| m)
                && (mask & (1 << free_col)) != 0
            {
                vec |= 1 << pivot_col;
            }
        }
        basis.push(vec);
    }
    basis
}

fn gaussian_gf2(goal: Lights, buttons: &[Button]) -> (u64, Vec<u64>) {
    let rows = buttons_for_light(goal, buttons);
    let n = buttons.len();
    let (r, pivots, frees) = eliminate(rows, n);
    let x0 = back_substitute(&r, &pivots, &frees);
    let null_basis = build_nullspace_basis(&r, &pivots, &frees);
    (x0, null_basis)
}

fn min_presses(machine: &Machine) -> usize {
    let (x0, basis) = gaussian_gf2(machine.goal, &machine.buttons);
    let num_free = basis.len();
    if num_free == 0 {
        return x0.count_ones() as usize;
    }
    let mut min_count = x0.count_ones() as usize;
    for s in 0..(1u64 << num_free) {
        let mut x = x0;
        for (i, &b) in basis.iter().enumerate() {
            if (s & (1 << i)) != 0 {
                x ^= b;
            }
        }
        min_count = min_count.min(x.count_ones() as usize);
    }
    min_count
}

fn process_file(filename: &str) -> io::Result<i64> {
    let contents = fs::read_to_string(filename)?;
    let machines: Vec<Machine> = contents
        .lines()
        .filter(|line| !line.is_empty())
        .filter_map(parse_line)
        .collect();

    let result: usize = machines.iter().map(min_presses).sum();
    Ok(result as i64)
}

fn display_duration(duration: u128) {
    print!("elapsed time: ");
    let duration_f = duration as f64;
    if duration_f < 1_000.0 {
        println!("{} ns", duration_f);
    } else if duration_f < 1_000_000.0 {
        println!("{} μs", duration_f / 1_000.0);
    } else if duration_f < 1_000_000_000.0 {
        println!("{} ms", duration_f / 1_000_000.0);
    } else {
        println!("{} s", duration_f / 1_000_000_000.0);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let filename = &args[1];
    let start = Instant::now();
    match process_file(filename) {
        Ok(result) => {
            let duration = start.elapsed().as_nanos();
            println!("result = {result}");
            display_duration(duration);
        }
        Err(e) => {
            eprintln!("error while processing file `{filename}': {e}");
            process::exit(1);
        }
    }
}
