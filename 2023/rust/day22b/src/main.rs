use std::collections::{HashMap, HashSet, VecDeque};
use std::env;
use std::fs;
use std::process;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
struct Point(u32, u32, u32);

type Brick = HashSet<Point>;

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
        let mut brick: Brick = HashSet::new();
        for z in sv[2]..=ev[2] {
            for y in sv[1]..=ev[1] {
                for x in sv[0]..=ev[0] {
                    brick.insert(Point(x, y, z));
                }
            }
        }
        bricks.push(brick);
    }
    bricks.sort_by(|a, b| {
        let min_a = a.iter().map(|&Point(_, _, z)| z).min().unwrap();
        let min_b = b.iter().map(|&Point(_, _, z)| z).min().unwrap();
        min_a.cmp(&min_b)
    });
    bricks
}

fn drop_bricks(bricks: &[Brick]) -> HashMap<usize, HashSet<usize>> {
    let mut occupied: HashMap<Point, usize> = HashMap::new();
    let mut supports: HashMap<usize, HashSet<usize>> = HashMap::new();
    for i in 0..bricks.len() {
        supports.insert(i, HashSet::new());
    }
    for i in 0..bricks.len() {
        let mut brick = bricks.get(i).unwrap().clone();
        let mut next_pos: Brick = brick
            .iter()
            .map(|&Point(x, y, z)| Point(x, y, z - 1))
            .collect();
        let mut intersected: HashSet<usize> = HashSet::new();
        for pos in next_pos.iter() {
            if occupied.contains_key(&pos) {
                intersected.insert(*occupied.get(&pos).unwrap());
            }
        }
        while intersected.is_empty() && !next_pos.iter().map(|&Point(_, _, z)| z).any(|x| x == 0) {
            brick = next_pos;
            next_pos = brick
                .iter()
                .map(|&Point(x, y, z)| Point(x, y, z - 1))
                .collect();
            intersected = HashSet::new();
            for pos in next_pos.iter() {
                if occupied.contains_key(&pos) {
                    intersected.insert(*occupied.get(&pos).unwrap());
                }
            }
        }
        let mut occupied_i: HashMap<Point, usize> = HashMap::new();
        for pos in brick.iter() {
            occupied_i.insert(*pos, i);
        }
        occupied.extend(&occupied_i);
        for parent in intersected.iter() {
            supports.entry(*parent).and_modify(|x| {
                x.insert(i);
            });
        }
    }
    supports
}

fn calc_supported(supports: &HashMap<usize, HashSet<usize>>) -> HashMap<usize, HashSet<usize>> {
    let mut supported: HashMap<usize, HashSet<usize>> = HashMap::new();
    for (parent, children) in supports.iter() {
        for child in children.iter() {
            supported
                .entry(*child)
                .and_modify(|x| {
                    x.insert(*parent);
                })
                .or_insert(HashSet::from([*parent]));
        }
    }
    supported
}

fn calc_safe(
    supports: &HashMap<usize, HashSet<usize>>,
    supported: &HashMap<usize, HashSet<usize>>,
) -> HashSet<usize> {
    let mut safe: HashSet<usize> = HashSet::new();
    for (parent, children) in supports.iter() {
        if children.is_empty()
            || children
                .iter()
                .all(|child| supported.get(child).unwrap().len() > 1)
        {
            safe.insert(*parent);
        }
    }
    safe
}

fn find_chain_reaction(
    supports: &HashMap<usize, HashSet<usize>>,
    supported: &HashMap<usize, HashSet<usize>>,
    start: usize,
) -> u32 {
    let mut count: u32 = 0;
    let mut q: VecDeque<usize> = VecDeque::new();
    let mut removed: HashSet<usize> = HashSet::new();
    q.push_back(start);
    while !q.is_empty() {
        let brick = q.pop_front().unwrap();
        removed.insert(brick);
        for child in supports.get(&brick).unwrap() {
            if supported.get(child).unwrap().difference(&removed).count() == 0 {
                count += 1;
                q.push_back(*child);
            }
        }
    }
    count
}

fn calc_chain_reactions(
    supports: &HashMap<usize, HashSet<usize>>,
    supported: &HashMap<usize, HashSet<usize>>,
    not_safe: &HashSet<usize>,
) -> u32 {
    let mut count: u32 = 0;
    for brick in not_safe.iter() {
        count += find_chain_reaction(supports, supported, *brick);
    }
    count
}

fn process(contents: &str) -> u32 {
    let bricks = parse_input(contents);
    let supports = drop_bricks(&bricks);
    let supported = calc_supported(&supports);
    let safe = calc_safe(&supports, &supported);
    let mut not_safe: HashSet<usize> = HashSet::new();
    for i in 0..bricks.len() {
        if !safe.contains(&i) {
            not_safe.insert(i);
        }
    }
    calc_chain_reactions(&supports, &supported, &not_safe)
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
