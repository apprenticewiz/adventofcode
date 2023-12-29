use std::env;
use std::fs;
use std::path;
use std::process;

#[derive(Clone, Copy, Debug)]
struct Position(i64, i64, i64);

const SAMPLE_AREA: ((i64, i64), (i64, i64)) = ((7, 7), (27, 27));
const INPUT_AREA: ((i64, i64), (i64, i64)) = (
    (200000000000000, 200000000000000),
    (400000000000000, 400000000000000),
);

#[derive(Clone, Copy, Debug)]
struct Velocity(i64, i64, i64);

#[derive(Clone, Copy, Debug)]
struct Hailstone {
    init_pos: Position,
    velocity: Velocity,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn check_hailstones(hailstones: &[Hailstone], test_area: &((i64, i64), (i64, i64))) -> u32 {
    let mut count: u32 = 0;
    for i in 0..hailstones.len() {
        let first = &hailstones[i];
        let a1x: f64 = first.init_pos.0 as f64;
        let a1y: f64 = first.init_pos.1 as f64;
        let b1x: f64 = first.velocity.0 as f64;
        let b1y: f64 = first.velocity.1 as f64;
        for j in (i + 1)..hailstones.len() {
            let second = &hailstones[j];
            let a2x: f64 = second.init_pos.0 as f64;
            let a2y: f64 = second.init_pos.1 as f64;
            let b2x: f64 = second.velocity.0 as f64;
            let b2y: f64 = second.velocity.1 as f64;
            let det_a = (-b1x * b2y) + (b2x * b1y);
            if det_a.abs() > 1.0e-6 {
                let det_at = (a1x * b2y) + (-a2x * b2y) + (-a1y * b2x) + (a2y * b2x);
                let det_as = (-a1y * b1x) + (a2y * b1x) + (a1x * b1y) + (-a2x * b1y);
                let t = det_at / det_a;
                let s = det_as / det_a;
                if t > 0.0 && s > 0.0 {
                    let x = a1x + b1x * t;
                    let y = a1y + b1y * t;
                    if x >= test_area.0.0 as f64 && x <= test_area.1.0 as f64
                        && y >= test_area.0.1 as f64 && y <= test_area.1.1 as f64
                    {
                        count += 1;
                    }
                }
            }
        }
    }
    count
}

fn process(contents: &str, test_area: &((i64, i64), (i64, i64))) -> u32 {
    let mut hailstones: Vec<Hailstone> = vec![];
    for line in contents.lines() {
        if let Some((pos_part, velo_part)) = line.split_once(" @ ") {
            let pos_vec = pos_part
                .split(",")
                .map(|x| x.trim().parse::<i64>().unwrap())
                .collect::<Vec<i64>>();
            let velo_vec = velo_part
                .split(",")
                .map(|x| x.trim().parse::<i64>().unwrap())
                .collect::<Vec<i64>>();
            let init_pos = Position(pos_vec[0], pos_vec[1], pos_vec[2]);
            let velocity = Velocity(velo_vec[0], velo_vec[1], velo_vec[2]);
            hailstones.push(Hailstone { init_pos, velocity });
        }
    }
    check_hailstones(&hailstones, test_area)
}

fn main() {
    if env::args().count() < 2 {
        usage();
    }
    let filename = env::args().nth(1).unwrap();
    let filepath = path::Path::new(&filename);
    if let Some(basename) = filepath.file_name() {
        let basename_str = basename.to_string_lossy().into_owned();
        let test_area = match basename_str.as_str() {
            "sample.txt" => SAMPLE_AREA,
            "input.txt" => INPUT_AREA,
            _ => panic!("filename must be either 'sample.txt' or 'input.txt'"),
        };
        let contents = fs::read_to_string(filename).expect("read of input file failed");
        let result = process(&contents, &test_area);
        println!("result = {result}");
    }
}
