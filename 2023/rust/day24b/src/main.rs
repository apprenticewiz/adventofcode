use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug)]
struct Position(i64, i64, i64);

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

// Credit to Tom Huntington for solution
// See: https://gist.github.com/tom-huntington/00065d3f5c52a900bfa99a6230470956

fn sub(u: &[f64], v: &[f64]) -> Vec<f64> {
    vec![u[0] - v[0], u[1] - v[1], u[2] - v[2]]
}

fn exterior3(u: &[f64], v: &[f64], w: &[f64]) -> f64 {
    u[0] * v[1] * w[2] + u[1] * v[2] * w[0] + u[2] * v[0] * w[1]
        - u[0] * v[2] * w[1]
        - u[1] * v[0] * w[2]
        - u[2] * v[1] * w[0]
}

fn exterior2(v: &[f64], w: &[f64]) -> Vec<f64> {
    vec![
        v[0] * w[1] - v[1] * w[0],
        v[1] * w[2] - v[2] * w[1],
        v[2] * w[0] - v[0] * w[2],
    ]
}

fn calc_start(hailstones: &[Hailstone]) -> u64 {
    let d: Vec<Vec<Vec<f64>>> = vec![
        vec![
            vec![
                hailstones[0].init_pos.0 as f64,
                hailstones[0].init_pos.1 as f64,
                hailstones[0].init_pos.2 as f64,
            ],
            vec![
                hailstones[0].velocity.0 as f64,
                hailstones[0].velocity.1 as f64,
                hailstones[0].velocity.2 as f64,
            ],
        ],
        vec![
            vec![
                hailstones[1].init_pos.0 as f64,
                hailstones[1].init_pos.1 as f64,
                hailstones[1].init_pos.2 as f64,
            ],
            vec![
                hailstones[1].velocity.0 as f64,
                hailstones[1].velocity.1 as f64,
                hailstones[1].velocity.2 as f64,
            ],
        ],
        vec![
            vec![
                hailstones[2].init_pos.0 as f64,
                hailstones[2].init_pos.1 as f64,
                hailstones[2].init_pos.2 as f64,
            ],
            vec![
                hailstones[2].velocity.0 as f64,
                hailstones[2].velocity.1 as f64,
                hailstones[2].velocity.2 as f64,
            ],
        ],
    ];
    let a: Vec<Vec<f64>> = vec![
        exterior2(&sub(&d[0][1], &d[1][1]), &sub(&d[0][0], &d[1][0])),
        exterior2(&sub(&d[0][1], &d[2][1]), &sub(&d[0][0], &d[2][0])),
        exterior2(&sub(&d[1][1], &d[2][1]), &sub(&d[1][0], &d[2][0])),
    ];
    let b: Vec<f64> = vec![
        -exterior3(&d[0][0], &d[0][1], &d[1][0]) - exterior3(&d[1][0], &d[1][1], &d[0][0]),
        -exterior3(&d[0][0], &d[0][1], &d[2][0]) - exterior3(&d[2][0], &d[2][1], &d[0][0]),
        -exterior3(&d[1][0], &d[1][1], &d[2][0]) - exterior3(&d[2][0], &d[2][1], &d[1][0]),
    ];
    let det_a =
        a[0][0] * a[1][1] * a[2][2] - a[0][0] * a[1][2] * a[2][1] - a[0][1] * a[1][0] * a[2][2]
            + a[0][1] * a[1][2] * a[2][0]
            + a[0][2] * a[1][0] * a[2][1]
            - a[0][2] * a[1][1] * a[2][0];
    let det_ax = b[0] * a[1][1] * a[2][2] - b[0] * a[1][2] * a[2][1] - a[0][1] * b[1] * a[2][2]
        + a[0][1] * a[1][2] * b[2]
        + a[0][2] * b[1] * a[2][1]
        - a[0][2] * a[1][1] * b[2];
    let det_ay = a[0][0] * b[1] * a[2][2] - a[0][0] * a[1][2] * b[2] - b[0] * a[1][0] * a[2][2]
        + b[0] * a[1][2] * a[2][0]
        + a[0][2] * a[1][0] * b[2]
        - a[0][2] * b[1] * a[2][0];
    let det_az = a[0][0] * a[1][1] * b[2] - a[0][0] * b[1] * a[2][1] - a[0][1] * a[1][0] * b[2]
        + a[0][1] * b[1] * a[2][0]
        + b[0] * a[1][0] * a[2][1]
        - b[0] * a[1][1] * a[2][0];
    let x = det_ax / det_a;
    let y = det_ay / det_a;
    let z = det_az / det_a;
    (x.ceil() + y.ceil() + z.ceil()) as u64
}

fn process(contents: &str) -> u64 {
    let mut hailstones: Vec<Hailstone> = vec![];
    for line in contents.lines() {
        if let Some((pos_part, velo_part)) = line.split_once(" @ ") {
            let pos_vec = pos_part
                .split(',')
                .map(|x| x.trim().parse::<i64>().unwrap())
                .collect::<Vec<i64>>();
            let velo_vec = velo_part
                .split(',')
                .map(|x| x.trim().parse::<i64>().unwrap())
                .collect::<Vec<i64>>();
            let init_pos = Position(pos_vec[0], pos_vec[1], pos_vec[2]);
            let velocity = Velocity(velo_vec[0], velo_vec[1], velo_vec[2]);
            hailstones.push(Hailstone { init_pos, velocity });
        }
    }
    calc_start(&hailstones)
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
