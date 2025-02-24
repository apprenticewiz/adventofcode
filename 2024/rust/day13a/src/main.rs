use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

use num_rational::Rational64;
use regex::Regex;

type Equations = ((u64, u64, u64), (u64, u64, u64));

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn parse_input(contents: &str) -> Vec<Equations> {
    let mut equations: Vec<Equations> = vec![];
    let re = Regex::new(r"X[+=](\d+), Y[+=](\d+)").unwrap();
    let mut bax = None;
    let mut bay = None;
    let mut bbx = None;
    let mut bby = None;
    let mut px = None;
    let mut py = None;
    for line in contents.lines() {
        if let Some(captures) = re.captures(line) {
            let x = captures[1].parse::<u64>().unwrap();
            let y = captures[2].parse::<u64>().unwrap();
            if line.starts_with("Button A") {
                bax = Some(x);
                bay = Some(y);
            } else if line.starts_with("Button B") {
                bbx = Some(x);
                bby = Some(y);
            } else if line.starts_with("Prize") {
                px = Some(x);
                py = Some(y);
            }
            if bax.is_some() && bay.is_some() && bbx.is_some() && bby.is_some() && px.is_some() && py.is_some() {
                equations.push(((bax.unwrap(), bbx.unwrap(), px.unwrap()), (bay.unwrap(), bby.unwrap(), py.unwrap())));
                bax = None;
                bay = None;
                bbx = None;
                bby = None;
                px = None;
                py = None;
            }
        }
    }
    equations
}

fn cramer(equations: &Equations) -> (Rational64, Rational64) {
    let a1 = Rational64::from_integer(equations.0.0.try_into().unwrap());
    let b1 = Rational64::from_integer(equations.0.1.try_into().unwrap());
    let c1 = Rational64::from_integer(equations.0.2.try_into().unwrap());
    let a2 = Rational64::from_integer(equations.1.0.try_into().unwrap());
    let b2 = Rational64::from_integer(equations.1.1.try_into().unwrap());
    let c2 = Rational64::from_integer(equations.1.2.try_into().unwrap());
    let x = (b2 * c1 - b1 * c2) / (a1 * b2 - a2 * b1);
    let y = (a1 * c2 - a2 * c1) / (a1 * b2 - a2 * b1);
    (x, y)
}

fn process(contents: &str) -> u64 {
    let mut result = 0;
    let equations = parse_input(contents);
    for equation in equations {
        let (a, b) = cramer(&equation);
        if *a.denom() == 1 && *b.denom() == 1 {
            result += (*a.numer() * 3 + *b.numer()) as u64
        }
    }
    result
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
