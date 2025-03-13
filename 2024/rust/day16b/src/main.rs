use std::cmp::{Ordering, Reverse};
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Orientation {
    Vertical,
    Horizontal,
}

type Position = (usize, usize);

type Grid = Vec<Vec<char>>;

type Node = (Position, Orientation);

type Graph = HashMap<Node, HashMap<Node, u32>>;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn scan_input(contents: &str) -> Grid {
    let mut grid: Grid = vec![];
    for line in contents.lines() {
        grid.push(line.chars().collect::<Vec<char>>());
    }
    grid
}

fn build_graph(grid: &Grid) -> Graph {
    let mut graph: Graph = HashMap::new();
    let dirs: [(isize, isize); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];
    for (row, row_chars) in grid.iter().enumerate() {
        for (col, ch) in row_chars.iter().enumerate() {
            if *ch != '#' {
                let pos: Position = (row, col);
                graph.insert(
                    (pos, Orientation::Vertical),
                    HashMap::from([((pos, Orientation::Horizontal), 1000)]),
                );
                graph.insert(
                    (pos, Orientation::Horizontal),
                    HashMap::from([((pos, Orientation::Vertical), 1000)]),
                );
                for dir in dirs {
                    let adj_pos: Position = (
                        (pos.0 as isize + dir.0) as usize,
                        (pos.1 as isize + dir.1) as usize,
                    );
                    if grid[adj_pos.0][adj_pos.1] != '#' {
                        let adj_orient: Orientation = match dir.0 % 2 {
                            0 => Orientation::Horizontal,
                            _ => Orientation::Vertical,
                        };
                        graph.entry((pos, adj_orient)).and_modify(|node| {
                            node.insert((adj_pos, adj_orient), 1);
                        });
                    }
                }
            }
        }
    }
    graph
}

fn get_endpoints(grid: &Grid) -> (Position, Position) {
    let mut start: Position = (0, 0);
    let mut end: Position = (0, 0);
    for (row, row_chars) in grid.iter().enumerate() {
        for (col, ch) in row_chars.iter().enumerate() {
            if *ch == 'S' {
                start = (row, col);
            } else if *ch == 'E' {
                end = (row, col);
            }
        }
    }
    (start, end)
}

fn find_best_seats(graph: &Graph, start: &Position, end: &Position) -> u32 {
    let start_node: Node = (*start, Orientation::Horizontal);
    let end_node1: Node = (*end, Orientation::Vertical);
    let end_node2: Node = (*end, Orientation::Horizontal);
    let mut pq: BinaryHeap<_> = BinaryHeap::from([Reverse((0, start_node, vec![start_node]))]);
    let mut costs: HashMap<Node, u32> = HashMap::new();
    let mut min_cost: u32 = u32::MAX;
    let mut best_paths: Vec<Vec<Node>> = vec![];
    let mut best_seats: HashSet<Position> = HashSet::new();
    while let Some(Reverse((cost, curr_node, path))) = pq.pop() {
        match cost.cmp(&min_cost) {
            Ordering::Less => {
                if curr_node == end_node1 || curr_node == end_node2 {
                    min_cost = cost;
                    best_paths = vec![path];
                    continue;
                }
            }
            Ordering::Equal => {
                if curr_node == end_node1 || curr_node == end_node2 {
                    best_paths.push(path);
                    continue;
                }
            }
            Ordering::Greater => {
                break;
            }
        }
        for (neighbor, edge_cost) in graph.get(&curr_node).unwrap() {
            let new_cost = cost + edge_cost;
            let neighbor_cost = if costs.contains_key(neighbor) {
                *costs.get(neighbor).unwrap()
            } else {
                u32::MAX
            };
            if new_cost <= neighbor_cost {
                let mut new_path = path.clone();
                new_path.push(*neighbor);
                pq.push(Reverse((new_cost, *neighbor, new_path)));
                costs.insert(*neighbor, new_cost);
            }
        }
    }
    for path in best_paths {
        for (pos, _) in path {
            best_seats.insert(pos);
        }
    }
    best_seats.len() as u32
}

fn process(contents: &str) -> u32 {
    let grid = scan_input(contents);
    let graph = build_graph(&grid);
    let (start, end) = get_endpoints(&grid);
    find_best_seats(&graph, &start, &end)
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
