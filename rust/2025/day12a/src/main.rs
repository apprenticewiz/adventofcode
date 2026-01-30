use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;

type Shape = Vec<(i32, i32)>;

#[derive(Debug, Clone)]
struct Region {
    width: usize,
    height: usize,
    counts: Vec<usize>,
}

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    std::process::exit(1);
}

#[inline]
fn to_idx(w: usize, r: i32, c: i32) -> usize {
    (r as usize) * w + (c as usize)
}

#[inline]
fn is_valid_placement(w: usize, h: usize, occupied: &[bool], shape: &Shape, r: i32, c: i32) -> bool {
    shape.iter().all(|&(dr, dc)| {
        let nr = r + dr;
        let nc = c + dc;
        nr >= 0
            && (nr as usize) < h
            && nc >= 0
            && (nc as usize) < w
            && !occupied[to_idx(w, nr, nc)]
    })
}

#[inline]
fn place_shape(w: usize, occupied: &mut [bool], shape: &Shape, r: i32, c: i32) {
    for &(dr, dc) in shape {
        occupied[to_idx(w, r + dr, c + dc)] = true;
    }
}

#[inline]
fn unplace_shape(w: usize, occupied: &mut [bool], shape: &Shape, r: i32, c: i32) {
    for &(dr, dc) in shape {
        occupied[to_idx(w, r + dr, c + dc)] = false;
    }
}

fn parse_shape(lines: &[&str]) -> Option<(usize, Shape)> {
    let first_line = lines.first()?;
    let (idx_str, _) = first_line.split_once(':')?;
    let idx: usize = idx_str.parse().ok()?;

    let mut coords = Vec::new();
    for (r, line) in lines.iter().skip(1).enumerate() {
        for (c, ch) in line.chars().enumerate() {
            if ch == '#' {
                coords.push((r as i32, c as i32));
            }
        }
    }
    Some((idx, coords))
}

fn parse_region(line: &str) -> Option<Region> {
    let (dims, counts_str) = line.split_once(':')?;
    let (w_str, h_str) = dims.split_once('x')?;
    let width: usize = w_str.parse().ok()?;
    let height: usize = h_str.parse().ok()?;
    let counts: Vec<usize> = counts_str
        .split_whitespace()
        .filter_map(|s| s.parse().ok())
        .collect();
    Some(Region {
        width,
        height,
        counts,
    })
}

fn parse_input(content: &str) -> (HashMap<usize, Shape>, Vec<Region>) {
    let lines: Vec<&str> = content.lines().collect();
    let mut shapes: HashMap<usize, Shape> = HashMap::new();
    let mut regions: Vec<Region> = Vec::new();

    let mut i = 0;
    // Parse shapes until we hit a region line (WxH: format)
    while i < lines.len() {
        let line = lines[i];
        if line.is_empty() {
            i += 1;
            continue;
        }
        // Check if this looks like a region line
        if line.contains('x') && line.contains(':') {
            let parts: Vec<&str> = line.split(':').collect();
            if parts.len() >= 2 && parts[0].contains('x') {
                break;
            }
        }
        // Check if this is a shape header (number followed by colon)
        if let Some((idx_str, _)) = line.split_once(':') {
            if idx_str.chars().all(|c| c.is_ascii_digit()) {
                let mut shape_lines = vec![line];
                i += 1;
                while i < lines.len() {
                    let next_line = lines[i];
                    if next_line.is_empty() {
                        break;
                    }
                    if next_line.contains(':') {
                        break;
                    }
                    shape_lines.push(next_line);
                    i += 1;
                }
                if let Some((idx, coords)) = parse_shape(&shape_lines) {
                    shapes.insert(idx, coords);
                }
                continue;
            }
        }
        i += 1;
    }

    // Parse regions
    while i < lines.len() {
        let line = lines[i];
        if !line.is_empty() {
            if let Some(region) = parse_region(line) {
                regions.push(region);
            }
        }
        i += 1;
    }

    (shapes, regions)
}

fn normalize_shape(shape: &Shape) -> Shape {
    if shape.is_empty() {
        return Vec::new();
    }
    let min_r = shape.iter().map(|&(r, _)| r).min().unwrap();
    let min_c = shape.iter().map(|&(_, c)| c).min().unwrap();
    let mut normalized: Shape = shape.iter().map(|&(r, c)| (r - min_r, c - min_c)).collect();
    normalized.sort();
    normalized
}

fn transformations(shape: &Shape) -> Vec<Shape> {
    let rot90 = |s: &Shape| -> Shape { s.iter().map(|&(r, c)| (-c, r)).collect() };
    let flip_h = |s: &Shape| -> Shape { s.iter().map(|&(r, c)| (-r, c)).collect() };

    let mut rots = vec![shape.clone()];
    for _ in 0..3 {
        rots.push(rot90(rots.last().unwrap()));
    }

    let mut all_trans: Vec<Shape> = rots.iter().cloned().collect();
    for rot in &rots {
        all_trans.push(flip_h(rot));
    }

    let mut seen: HashSet<Shape> = HashSet::new();
    let mut unique = Vec::new();
    for s in all_trans {
        let norm = normalize_shape(&s);
        if seen.insert(norm.clone()) {
            unique.push(norm);
        }
    }
    unique
}

fn try_place_helper(
    w: usize,
    h: usize,
    counts: &mut [usize],
    shape_idx: usize,
    occupied: &mut [bool],
    trans_cache: &[Vec<Shape>],
) -> bool {
    // Find next shape to place (starting from shape_idx)
    let mut idx = shape_idx;
    while idx < counts.len() && counts[idx] == 0 {
        idx += 1;
    }
    
    if idx >= counts.len() {
        return true; // All shapes placed
    }

    let orientations = &trans_cache[idx];
    
    for orientation in orientations {
        for r in 0..h as i32 {
            for c in 0..w as i32 {
                if is_valid_placement(w, h, occupied, orientation, r, c) {
                    place_shape(w, occupied, orientation, r, c);
                    counts[idx] -= 1;

                    if try_place_helper(w, h, counts, idx, occupied, trans_cache) {
                        // Restore state before returning (for correctness if caller needs it)
                        counts[idx] += 1;
                        unplace_shape(w, occupied, orientation, r, c);
                        return true;
                    }

                    counts[idx] += 1;
                    unplace_shape(w, occupied, orientation, r, c);
                }
            }
        }
    }

    false
}

fn try_place(w: usize, h: usize, shapes: &HashMap<usize, Shape>, present_counts: &[usize]) -> bool {
    let total_area = w * h;
    let required_area: usize = present_counts
        .iter()
        .enumerate()
        .filter_map(|(idx, &c)| shapes.get(&idx).map(|s| c * s.len()))
        .sum();

    if required_area > total_area {
        return false;
    }

    // Build transformation cache indexed by shape index
    let max_idx = present_counts.len();
    let trans_cache: Vec<Vec<Shape>> = (0..max_idx)
        .map(|idx| {
            if present_counts[idx] > 0 {
                shapes.get(&idx).map(|s| transformations(s)).unwrap_or_default()
            } else {
                Vec::new()
            }
        })
        .collect();

    let mut occupied = vec![false; w * h];
    let mut counts = present_counts.to_vec();
    
    try_place_helper(w, h, &mut counts, 0, &mut occupied, &trans_cache)
}

fn can_fit_region(shapes: &HashMap<usize, Shape>, region: &Region) -> bool {
    try_place(region.width, region.height, shapes, &region.counts)
}

fn process(content: &str) -> usize {
    let (shapes, regions) = parse_input(content);
    regions
        .iter()
        .filter(|region| can_fit_region(&shapes, region))
        .count()
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
    let start = std::time::Instant::now();
    match fs::read_to_string(filename) {
        Ok(content) => {
            let result = process(&content);
            let duration = start.elapsed().as_nanos();
            println!("result = {result}");
            display_duration(duration);
        }
        Err(e) => {
            eprintln!("error while processing file `{filename}': {e}");
            std::process::exit(1);
        }
    }
}
