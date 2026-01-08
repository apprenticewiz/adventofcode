use std::env;
use std::fs;
use std::io;
use std::process;
use std::time::Instant;

type Position = (i64, i64, i64);

struct UnionFind {
    parent: Vec<usize>,
    size: Vec<usize>,
}

impl UnionFind {
    fn new(n: usize) -> Self {
        Self {
            parent: (0..n).collect(),
            size: vec![1; n],
        }
    }

    fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            self.parent[x] = self.find(self.parent[x]);
        }
        self.parent[x]
    }

    fn union(&mut self, a: usize, b: usize) {
        let mut ra = self.find(a);
        let mut rb = self.find(b);
        if ra == rb {
            return;
        }
        if self.size[ra] < self.size[rb] {
            std::mem::swap(&mut ra, &mut rb);
        }
        self.parent[rb] = ra;
        self.size[ra] += self.size[rb];
    }

    fn component_sizes(&mut self) -> Vec<usize> {
        let mut sizes = vec![0; self.parent.len()];
        for i in 0..self.parent.len() {
            let r = self.find(i);
            sizes[r] += 1;
        }
        sizes.into_iter().filter(|s| *s > 0).collect()
    }
}

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_points(contents: &str) -> Vec<Position> {
    let mut points: Vec<Position> = vec![];
    contents.lines().for_each(|line| {
        let parts: Vec<&str> = line.split(',').collect();
        let x = parts[0].parse::<i64>().unwrap();
        let y = parts[1].parse::<i64>().unwrap();
        let z = parts[2].parse::<i64>().unwrap();
        points.push((x, y, z));
    });
    points
}

fn euclidian_dist(p1: Position, p2: Position) -> f64 {
    let (x1, y1, z1) = p1;
    let (x2, y2, z2) = p2;
    let xx = (x1 - x2).pow(2) as f64;
    let yy = (y1 - y2).pow(2) as f64;
    let zz = (z1 - z2).pow(2) as f64;
    (xx + yy + zz).sqrt()
}

fn process_file(filename: &str) -> io::Result<i64> {
    let contents = fs::read_to_string(filename)?;
    let points = parse_points(&contents);
    let n = points.len();
    let mut uf = UnionFind::new(n);
    let mut distances: Vec<((usize, usize), f64)> = vec![];
    for i in 0..points.len() {
        for j in (i + 1)..points.len() {
            distances.push(((i, j), euclidian_dist(points[i], points[j])));
        }
    }
    distances.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
    for ((i, j), _) in distances.iter().take(1000) {
        uf.union(*i, *j);
    }
    let mut sizes = uf.component_sizes();
    sizes.sort_unstable();
    let result: i64 = sizes.iter().rev().take(3).map(|s| *s as i64).product();
    Ok(result)
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
