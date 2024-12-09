use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn process(contents: &str) -> u64 {
    let split_line = |line: &str| {
        let parts: Vec<&str> = line.split(':').collect();
        let lhs = parts[0].trim().parse::<u64>().unwrap();
        let rhs = parts[1]
            .split_whitespace()
            .map(|s| s.parse::<u64>().unwrap())
            .collect::<Vec<_>>();
        (lhs, rhs)
    };

    fn generate_equations(numbers: Vec<u64>) -> Vec<String> {
        if numbers.len() == 1 {
            vec![numbers[0].to_string()]
        } else {
            let mut equations = Vec::new();
            let first = numbers[0];
            let rest = generate_equations(numbers[1..].to_vec());
            for eqn in rest {
                equations.push(format!("{} + {}", first, eqn));
                equations.push(format!("{} * {}", first, eqn));
            }
            equations
        }
    }

    let solve = |expr: Vec<&str>| -> String {
        let mut stack: Vec<String> = Vec::new();
        for token in expr {
            match token {
                "+" | "*" => stack.push(token.to_string()),
                _ => stack.push(token.to_string()),
            }
            if stack.len() == 3 {
                let x = stack.pop().unwrap();
                let op = stack.pop().unwrap();
                let y = stack.pop().unwrap();
                match op.as_str() {
                    "+" => {
                        let sum = x.parse::<u64>().unwrap() + y.parse::<u64>().unwrap();
                        stack.push(format!("{}", sum));
                    }
                    "*" => {
                        let prod = x.parse::<u64>().unwrap() * y.parse::<u64>().unwrap();
                        stack.push(format!("{}", prod));
                    }
                    _ => panic!("unexpected operator found on stack")
                }
            }
        }
        stack.pop().unwrap_or_else(|| panic!("Stack is empty after solving expression"))
    };

    let valid_solutions = contents.lines().fold(Vec::new(), |mut valid, line| {
        let (lhs, rhs) = split_line(line);
        let rhs_eqns = generate_equations(rhs);
        let solutions: Vec<u64> = rhs_eqns
            .iter()
            .map(|eqn| {
                solve(eqn.split_whitespace().collect::<Vec<_>>())
                    .parse::<u64>()
                    .unwrap()
            })
            .collect();

        if solutions.contains(&lhs) {
            valid.push(lhs);
        }
        valid
    });

    valid_solutions.iter().sum()
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
