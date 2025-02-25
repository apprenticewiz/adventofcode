use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

use regex::Regex;

const NUM_STEPS: u32 = 100;

struct Robot {
    position: (i32, i32),
    velocity: (i32, i32),
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn parse_input(contents: &str) -> Vec<Robot> {
    let mut robots = vec![];
    let re = Regex::new(r"p=(\d+),(\d+) v=([-]?\d+),([-]?\d+)").unwrap();
    for line in contents.lines() {
        if let Some(captures) = re.captures(line) {
            let px = captures[1].parse::<i32>().unwrap();
            let py = captures[2].parse::<i32>().unwrap();
            let dx = captures[3].parse::<i32>().unwrap();
            let dy = captures[4].parse::<i32>().unwrap();
            robots.push(Robot {
                position: (px, py),
                velocity: (dx, dy),
            });
        }
    }
    robots
}

fn step(extents: (i32, i32), robots: &[Robot]) -> Vec<Robot> {
    let mut next_robots = vec![];
    for robot in robots {
        let mut next_robot = Robot {
            position: (
                robot.position.0 + robot.velocity.0,
                robot.position.1 + robot.velocity.1,
            ),
            velocity: robot.velocity,
        };
        if next_robot.position.0 < 0 {
            next_robot.position.0 += extents.0;
        } else if next_robot.position.0 >= extents.0 {
            next_robot.position.0 -= extents.0;
        }
        if next_robot.position.1 < 0 {
            next_robot.position.1 += extents.1;
        } else if next_robot.position.1 >= extents.1 {
            next_robot.position.1 -= extents.1;
        }
        next_robots.push(next_robot);
    }
    next_robots
}

fn safety_factor(extents: (i32, i32), robots: &[Robot]) -> u32 {
    let mid_x = extents.0 / 2;
    let mid_y = extents.1 / 2;
    let mut quadrants: [u32; 4] = [0; 4];
    for robot in robots {
        if robot.position.0 < mid_x && robot.position.1 < mid_y {
            quadrants[0] += 1;
        } else if robot.position.0 > mid_x && robot.position.1 < mid_y {
            quadrants[1] += 1;
        } else if robot.position.0 > mid_x && robot.position.1 > mid_y {
            quadrants[2] += 1;
        } else if robot.position.0 < mid_x && robot.position.1 > mid_y {
            quadrants[3] += 1;
        }
    }
    quadrants[0] * quadrants[1] * quadrants[2] * quadrants[3]
}

fn process(contents: &str, extents: (i32, i32)) -> u32 {
    let mut robots = parse_input(contents);
    for _ in 0..NUM_STEPS {
        robots = step(extents, &robots);
    }
    safety_factor(extents, &robots)
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
    let mut extents = (101, 103);
    if filename.contains("sample") {
        extents = (11, 7);
    }
    let contents = fs::read_to_string(filename).expect("read of input file failed");
    let result = process(&contents, extents);
    let duration = calc_runtime(&start_time);
    println!("result = {result}  {duration}");
}
