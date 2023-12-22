use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Point(u32, u32, u32);

#[derive(Clone, Copy, Debug, PartialEq)]
struct Brick {
    start: Point,
    end: Point,
}

impl Brick {
    // this is really dumb, and there is surely a better way to do this
    fn intersects(&self, other: Brick) -> bool {
        for x1 in self.start.0..=self.end.0 {
            for y1 in self.start.1..=self.end.1 {
                for z1 in self.start.2..=self.end.2 {
                    let p1 = Point(x1, y1, z1);
                    for x2 in other.start.0..=other.end.0 {
                        for y2 in other.start.1..=other.end.1 {
                            for z2 in other.start.2..=other.end.2 {
                                let p2 = Point(x2, y2, z2);
                                if p1 == p2 {
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
        }
        false
    }
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn parse_input(contents: &str) -> Vec<Brick> {
    let mut bricks: Vec<Brick> = vec![];
    for line in contents.lines() {
        let (start_str, end_str) = line.split_once('~').unwrap();
        let sv = start_str
            .split(',')
            .map(|x| x.parse::<u32>().unwrap())
            .collect::<Vec<u32>>();
        let ev = end_str
            .split(',')
            .map(|x| x.parse::<u32>().unwrap())
            .collect::<Vec<u32>>();
        let brick: Brick = Brick {
            start: Point(sv[0], sv[1], sv[2]),
            end: Point(ev[0], ev[1], ev[2]),
        };
        bricks.push(brick);
    }
    bricks.sort_by_key(|x| x.start.2);
    bricks
}

fn drop_bricks(bricks: &[Brick]) -> Vec<Brick> {
    let mut dropped: u32 = 0;
    let mut working_bricks = bricks.to_vec();
    loop {
        let mut next_bricks: Vec<Brick> = vec![];
        for i in 0..working_bricks.len() {
            let brick = &working_bricks[i];
            let new_brick: Brick = Brick {
                start: Point(brick.start.0, brick.start.1, brick.start.2 - 1),
                end: Point(brick.end.0, brick.end.1, brick.end.2 - 1),
            };
            let mut intersected = false;
            for other_brick in working_bricks.iter().take(i) {
                if new_brick.intersects(*other_brick) {
                    intersected = true;
                    break;
                }
            }
            if !intersected && new_brick.start.2 != 0 && new_brick.end.2 != 0 {
                next_bricks.push(new_brick);
                dropped += 1;
            } else {
                next_bricks.push(*brick);
            }
        }
        working_bricks = next_bricks;
        if dropped == 0 {
            break;
        } else {
            dropped = 0;
        }
    }
    working_bricks
}

fn count_safes(bricks: &Vec<Brick>) -> u32 {
    let mut count: u32 = 0;
    for i in 0..bricks.len() {
        let mut working_bricks = bricks.clone();
        working_bricks.remove(i);
        let dropped_bricks = drop_bricks(&working_bricks);
        if dropped_bricks == working_bricks {
            count += 1;
        }
    }
    count
}

fn process(contents: &str) -> u32 {
    let mut bricks = parse_input(contents);
    bricks = drop_bricks(&bricks);
    count_safes(&bricks)
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
