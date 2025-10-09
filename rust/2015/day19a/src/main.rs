use std::collections::HashSet;
use std::env;
use std::fs;
use std::io;
use std::process;

type Rule = (String, String);

type Molecule = String;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_file(filename: &str) -> io::Result<(Vec<Rule>, Molecule)> {
    let mut rules: Vec<Rule> = vec![];
    let content = fs::read_to_string(filename)?;
    let mut line_it = content.lines();
    loop {
        match line_it.next() {
            Some(line) => {
                if line.is_empty() {
                    break;
                } else {
                    let (lhs, rhs) = line.split_once(" => ").unwrap();
                    rules.push((lhs.to_string(), rhs.to_string()));
                }
            }
            None => panic!("malformed input - input ended prematurely"),
        }
    }
    let molecule = line_it.next().unwrap().to_string();
    Ok((rules, molecule))
}

fn apply_rule(rule: (&str, &str), molecule: &str) -> Vec<Molecule> {
    let (lhs, rhs) = rule;
    let mut results = vec![];
    let mol_len = molecule.len();
    for i in 0..=mol_len - lhs.len() {
        if molecule[i..].starts_with(lhs) {
            let pre = &molecule[..i];
            let suf = &molecule[i + lhs.len()..];
            let new_molecule = format!("{}{}{}", pre, rhs, suf);
            results.push(new_molecule);
        }
    }
    results
}

fn distinct_molecules(rules: &[Rule], molecule: &Molecule) -> HashSet<Molecule> {
    let mut result: HashSet<Molecule> = HashSet::new();
    for rule in rules.iter() {
        let (lhs, rhs) = rule;
        let new_molecules = apply_rule((lhs, rhs), molecule);
        result.extend(new_molecules);
    }
    result
}

fn process_file(filename: &str) -> io::Result<u32> {
    let (rules, molecule) = parse_file(filename)?;
    let result = distinct_molecules(&rules, &molecule);
    Ok(result.len() as u32)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let filename = &args[1];
    match process_file(filename) {
        Ok(result) => println!("result = {result}"),
        Err(e) => {
            eprintln!("error while processing file `{filename}': {e}");
            process::exit(1);
        }
    }
}
