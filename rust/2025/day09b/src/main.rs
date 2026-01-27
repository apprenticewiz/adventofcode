use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;
use std::process;
use std::time::Instant;

type Position = (i64, i64);
type Segment = (i64, i64);

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn collect_horz(edges: &[(Position, Position)]) -> Vec<(i64, Segment)> {
    edges
        .iter()
        .filter_map(|&((x1, y1), (x2, y2))| {
            if y1 == y2 {
                Some((y1, (x1.min(x2), x1.max(x2))))
            } else {
                None
            }
        })
        .collect()
}

fn merge_segments(mut segs: Vec<Segment>) -> Vec<Segment> {
    if segs.is_empty() {
        return vec![];
    }
    segs.sort_unstable_by_key(|&(a, _)| a);

    let mut result = Vec::new();
    let mut current = segs[0];

    for &(a, b) in &segs[1..] {
        if a <= current.1 + 1 {
            current.1 = current.1.max(b);
        } else {
            result.push(current);
            current = (a, b);
        }
    }
    result.push(current);
    result
}

fn build_vertical_edge_map(
    edges: &[(Position, Position)],
    min_y: i64,
    max_y: i64,
) -> HashMap<i64, Vec<i64>> {
    let mut map: HashMap<i64, Vec<i64>> = HashMap::new();

    for &((x1, y1), (x2, y2)) in edges {
        if x1 == x2 {
            let y_lo = y1.min(y2);
            let y_hi = y1.max(y2);
            let in_range_start = min_y.max(y_lo);
            let in_range_end = max_y.min(y_hi - 1);

            for y in in_range_start..=in_range_end {
                map.entry(y).or_default().push(x1);
            }
        }
    }
    for crossings in map.values_mut() {
        crossings.sort_unstable();
    }

    map
}

fn pair_up(xs: &[i64]) -> Vec<Segment> {
    xs.chunks_exact(2)
        .map(|chunk| (chunk[0], chunk[1]))
        .collect()
}

fn valid_rectangle(
    p1: Position,
    p2: Position,
    valid_x_ranges_map: &HashMap<i64, Vec<Segment>>,
) -> bool {
    let (x1, y1) = p1;
    let (x2, y2) = p2;
    let x_lo = x1.min(x2);
    let x_hi = x1.max(x2);
    let y_lo = y1.min(y2);
    let y_hi = y1.max(y2);

    let valid_for_y = |y: i64| -> bool {
        if let Some(ranges) = valid_x_ranges_map.get(&y) {
            ranges.iter().any(|&(a, b)| a <= x_lo && x_hi <= b)
        } else {
            false
        }
    };
    if !valid_for_y(y_lo) || !valid_for_y(y_hi) {
        return false;
    }
    (y_lo..=y_hi).all(valid_for_y)
}

fn process_file(filename: &str) -> io::Result<i64> {
    let contents = fs::read_to_string(filename)?;
    let reds: Vec<Position> = contents
        .lines()
        .map(|line| {
            let parts = line.split_once(',').unwrap();
            let x = parts.0.parse::<i64>().unwrap();
            let y = parts.1.parse::<i64>().unwrap();
            (x, y)
        })
        .collect();
    let edges: Vec<(Position, Position)> = reds
        .iter()
        .copied()
        .zip(reds.iter().copied().cycle().skip(1))
        .take(reds.len())
        .collect();
    let horz_list = collect_horz(&edges);
    let mut horz_map: HashMap<i64, Vec<Segment>> = HashMap::new();
    for (y, seg) in horz_list {
        horz_map.entry(y).or_default().push(seg);
    }
    let horz_map: HashMap<i64, Vec<Segment>> = horz_map
        .into_iter()
        .map(|(y, segs)| (y, merge_segments(segs)))
        .collect();
    let min_y = reds.iter().map(|&(_, y)| y).min().unwrap();
    let max_y = reds.iter().map(|&(_, y)| y).max().unwrap();
    let vertical_edge_map = build_vertical_edge_map(&edges, min_y, max_y);
    let valid_x_ranges_map: HashMap<i64, Vec<Segment>> = (min_y..=max_y)
        .map(|y| {
            let horz_segs = horz_map.get(&y).cloned().unwrap_or_default();
            let crossings = vertical_edge_map.get(&y).cloned().unwrap_or_default();
            let interior_ranges = pair_up(&crossings);
            let mut all_ranges = horz_segs;
            all_ranges.extend(interior_ranges);
            let merged = merge_segments(all_ranges);
            (y, merged)
        })
        .collect();
    let mut candidates: Vec<(u64, Position, Position)> = Vec::new();
    for (i, &p1) in reds.iter().enumerate() {
        for &p2 in &reds[i + 1..] {
            let (x1, y1) = p1;
            let (x2, y2) = p2;
            if x1 != x2 && y1 != y2 {
                let width = x1.abs_diff(x2) + 1;
                let height = y1.abs_diff(y2) + 1;
                let area = width * height;
                candidates.push((area, p1, p2));
            }
        }
    }
    candidates.sort_unstable_by(|a, b| b.0.cmp(&a.0));
    let best = candidates
        .into_iter()
        .find(|&(_, p1, p2)| valid_rectangle(p1, p2, &valid_x_ranges_map))
        .map(|(area, _, _)| area as i64)
        .unwrap_or(0);
    Ok(best)
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
