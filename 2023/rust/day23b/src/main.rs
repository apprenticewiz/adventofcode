use std::collections::{HashMap, HashSet, VecDeque};
use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Position(i32, i32);

#[derive(Debug)]
struct GraphBuilderState {
    pos: Position,
    visited: HashSet<Position>,
    prev: Position,
    last: u32,
    steps: u32,
}

#[derive(Debug)]
struct GraphTraversalState {
    node: u32,
    path: Vec<u32>,
    steps: u32,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_grid(contents: &str) -> Vec<Vec<char>> {
    let mut grid: Vec<Vec<char>> = vec![];
    for line in contents.lines() {
        grid.push(line.chars().collect::<Vec<char>>());
    }
    grid
}

fn in_bounds(pos: Position, extents: Position) -> bool {
    pos.0 >= 0 && pos.0 < extents.0 && pos.1 >= 0 && pos.1 < extents.1
}

fn build_graph(grid: &[Vec<char>]) -> HashMap<u32, HashSet<(u32, u32)>> {
    let start = Position(0, 1);
    let num_rows = grid.len() as i32;
    let num_cols = grid[0].len() as i32;
    let extents = Position(num_rows, num_cols);
    let end = Position(num_rows - 1, num_cols - 2);
    let directions = [
        Position(-1, 0),
        Position(1, 0),
        Position(0, -1),
        Position(0, 1),
    ];
    let mut branches: HashMap<Position, u32> = HashMap::from([(start, 0), (end, 1)]);
    let mut graph: HashMap<u32, HashSet<(u32, u32)>> =
        HashMap::from([(0, HashSet::new()), (1, HashSet::new())]);
    let init_state: GraphBuilderState = GraphBuilderState {
        pos: start,
        visited: HashSet::new(),
        prev: start,
        last: 0,
        steps: 0,
    };
    let mut stack: VecDeque<GraphBuilderState> = VecDeque::new();
    stack.push_back(init_state);
    while !stack.is_empty() {
        let curr_state = stack.pop_back().unwrap();
        let pos = curr_state.pos;
        let mut visited = curr_state.visited.clone();
        visited.insert(pos);
        let mut count = 0;
        let prev = curr_state.prev;
        for direction in directions.iter() {
            let next_pos = Position(pos.0 + direction.0, pos.1 + direction.1);
            if in_bounds(next_pos, extents)
                && grid[next_pos.0 as usize][next_pos.1 as usize] != '#'
                && next_pos != prev
            {
                count += 1;
            }
        }
        let mut last = curr_state.last;
        let mut steps = curr_state.steps;
        if count > 1 {
            let curr = branches.keys().count() as u32;
            branches.insert(curr_state.pos, curr);
            graph
                .entry(curr)
                .and_modify(|x| {
                    x.insert((last, steps));
                })
                .or_insert(HashSet::from([(last, steps)]));
            graph
                .entry(last)
                .and_modify(|x| {
                    x.insert((curr, steps));
                })
                .or_insert(HashSet::from([(curr, steps)]));
            last = curr;
            steps = 0;
        }
        for direction in directions.iter() {
            let next_pos = Position(pos.0 + direction.0, pos.1 + direction.1);
            if in_bounds(next_pos, extents) && next_pos != prev && branches.contains_key(&next_pos)
            {
                let curr = branches.get(&next_pos).unwrap();
                graph
                    .entry(*curr)
                    .and_modify(|x| {
                        x.insert((last, steps + 1));
                    })
                    .or_insert(HashSet::from([(last, steps + 1)]));
                graph
                    .entry(last)
                    .and_modify(|x| {
                        x.insert((*curr, steps + 1));
                    })
                    .or_insert(HashSet::from([(*curr, steps + 1)]));
            } else if in_bounds(next_pos, extents)
                && grid[next_pos.0 as usize][next_pos.1 as usize] != '#'
                && !visited.contains(&next_pos)
            {
                stack.push_back(GraphBuilderState {
                    pos: next_pos,
                    visited: visited.clone(),
                    prev: pos,
                    last,
                    steps: steps + 1,
                });
            }
        }
    }
    graph
}

fn find_longest(graph: &HashMap<u32, HashSet<(u32, u32)>>) -> u32 {
    const SOURCE: u32 = 0;
    const DEST: u32 = 1;
    let mut stack: VecDeque<GraphTraversalState> = VecDeque::new();
    let mut costs: Vec<u32> = vec![];
    stack.push_back(GraphTraversalState {
        node: SOURCE,
        path: vec![],
        steps: 0,
    });
    while !stack.is_empty() {
        let state = stack.pop_back().unwrap();
        if state.node == DEST {
            costs.push(state.steps);
        } else {
            let mut path = state.path.clone();
            path.push(state.node);
            for (dest, cost) in graph.get(&state.node).unwrap() {
                if !path.contains(dest) {
                    stack.push_back(GraphTraversalState {
                        node: *dest,
                        path: path.clone(),
                        steps: state.steps + cost,
                    });
                }
            }
        }
    }
    *costs.iter().max().unwrap()
}

fn process(contents: &str) -> u32 {
    let grid = build_grid(contents);
    let graph = build_graph(&grid);
    find_longest(&graph)
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
