use std::collections::{HashSet, VecDeque};
use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

type Position = (u32, u32);

#[derive(Clone)]
struct Warehouse {
    dimensions: (u32, u32),
    robot: Position,
    boxes: HashSet<Position>,
    obstacles: HashSet<Position>,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn scan_input(contents: &str) -> (Warehouse, Vec<char>) {
    let mut moves: Vec<char> = vec![];
    let mut obstacles: HashSet<Position> = HashSet::new();
    let mut boxes: HashSet<Position> = HashSet::new();
    let mut robot: Position = (0, 0);
    let mut dimensions: (u32, u32) = (0, 0);
    let mut horiz = 0;
    for (row, line) in contents.lines().enumerate() {
        if line.starts_with("#") {
            if line.chars().all(|x| x == '#') {
                horiz += 1;
            }
            for (col, ch) in line.chars().enumerate() {
                match ch {
                    '#' => {
                        obstacles.insert((row as u32, col as u32));
                    }
                    'O' => {
                        boxes.insert((row as u32, col as u32));
                    }
                    '@' => robot = (row as u32, col as u32),
                    _ => (),
                }
            }
            if horiz == 2 {
                dimensions = ((row + 1) as u32, line.chars().count() as u32);
            }
        } else if line.starts_with(['^', 'v', '<', '>']) {
            moves.append(&mut line.chars().collect::<Vec<char>>());
        }
    }
    (
        Warehouse {
            dimensions,
            robot,
            boxes,
            obstacles,
        },
        moves,
    )
}

fn move_robot(warehouse: &Warehouse, dir: (i32, i32)) -> Warehouse {
    let next_robot_pos = (
        (warehouse.robot.0 as i32 + dir.0) as u32,
        (warehouse.robot.1 as i32 + dir.1) as u32,
    );
    if warehouse.obstacles.contains(&next_robot_pos) {
        warehouse.clone()
    } else if warehouse.boxes.contains(&next_robot_pos) {
        let mut boxes_to_move: HashSet<Position> = HashSet::new();
        let initial_box = next_robot_pos;
        let mut q: VecDeque<Position> = VecDeque::new();
        q.push_back(initial_box);
        while let Some(curr_box) = q.pop_front() {
            boxes_to_move.insert(curr_box);
            let next_box = (
                (curr_box.0 as i32 + dir.0) as u32,
                (curr_box.1 as i32 + dir.1) as u32,
            );
            if warehouse.boxes.contains(&next_box) {
                q.push_back(next_box);
            }
        }
        let mut working_set: HashSet<Position> = HashSet::new();
        for pos in boxes_to_move.iter() {
            working_set.insert(((pos.0 as i32 + dir.0) as u32, (pos.1 as i32 + dir.1) as u32));
        }
        if warehouse.obstacles.intersection(&working_set).count() > 0 {
            return warehouse.clone();
        } else {
            let stationary_boxes: HashSet<Position> = warehouse
                .boxes
                .difference(&boxes_to_move)
                .cloned()
                .collect();
            let moved_boxes: HashSet<Position> = boxes_to_move
                .iter()
                .map(|pos| ((pos.0 as i32 + dir.0) as u32, (pos.1 as i32 + dir.1) as u32))
                .collect();
            let new_boxes: HashSet<Position> =
                stationary_boxes.union(&moved_boxes).cloned().collect();
            return Warehouse {
                dimensions: warehouse.dimensions,
                robot: next_robot_pos,
                boxes: new_boxes,
                obstacles: warehouse.obstacles.clone(),
            };
        }
    } else {
        return Warehouse {
            dimensions: warehouse.dimensions,
            robot: next_robot_pos,
            boxes: warehouse.boxes.clone(),
            obstacles: warehouse.obstacles.clone(),
        };
    }
}

fn process_moves(warehouse: &Warehouse, moves: &[char]) -> Warehouse {
    let mut new_warehouse = Warehouse {
        dimensions: warehouse.dimensions,
        robot: warehouse.robot,
        boxes: warehouse.boxes.clone(),
        obstacles: warehouse.obstacles.clone(),
    };
    for curr_move in moves {
        let dir: (i32, i32) = match curr_move {
            '^' => (-1, 0),
            'v' => (1, 0),
            '<' => (0, -1),
            '>' => (0, 1),
            _ => panic!("invalid move"),
        };
        new_warehouse = move_robot(&new_warehouse, dir);
    }
    new_warehouse
}

fn process(contents: &str) -> u32 {
    let (warehouse, moves) = scan_input(contents);
    let final_state = process_moves(&warehouse, &moves);
    final_state
        .boxes
        .iter()
        .map(|pos| pos.0 * 100 + pos.1)
        .sum::<u32>()
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
    let contents = fs::read_to_string(filename).expect("read of input file failed");
    let result = process(&contents);
    let duration = calc_runtime(&start_time);
    println!("result = {result}  {duration}");
}
