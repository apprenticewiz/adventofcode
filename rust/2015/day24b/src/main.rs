use std::env;
use std::fs;
use std::io;
use std::process;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn valid_combinations(
    package_weights: &[i64],
    num_groups: usize,
    target_weight_per_group: i64,
) -> Vec<Vec<i64>> {
    let package_count = package_weights.len();
    let max_packages = package_count / num_groups;
    let mut all_valid = vec![];
    for n in 1..=max_packages {
        let mut valid_for_n: Vec<Vec<i64>> = combinations(n, package_weights)
            .into_iter()
            .filter(|combo| combo.iter().sum::<i64>() == target_weight_per_group)
            .collect();
        all_valid.append(&mut valid_for_n);
    }
    all_valid
}

fn combinations<T: Clone>(k: usize, xs: &[T]) -> Vec<Vec<T>> {
    if k == 0 {
        return vec![vec![]];
    }
    if k > xs.len() {
        return vec![];
    }
    let mut result = vec![];
    if let Some((first, rest)) = xs.split_first() {
        for mut c in combinations(k - 1, rest) {
            c.insert(0, first.clone());
            result.push(c);
        }
        result.extend(combinations(k, rest));
    }
    result
}

fn distribute(packages: &[i64], num_groups: usize) -> Vec<i64> {
    let total_weight: i64 = packages.iter().sum();
    let target_weight_per_group = total_weight / num_groups as i64;
    let valid_combos = valid_combinations(packages, num_groups, target_weight_per_group);
    if valid_combos.is_empty() {
        panic!("no valid combinations found");
    }
    valid_combos
        .into_iter()
        .min_by(|a, b| {
            let prod_a: i64 = a.iter().product();
            let prod_b: i64 = b.iter().product();
            prod_a.cmp(&prod_b)
        })
        .unwrap()
}

fn process_file(filename: &str) -> io::Result<i64> {
    let content = fs::read_to_string(filename)?;
    let packages: Vec<i64> = content
        .lines()
        .filter_map(|x| x.parse::<i64>().ok())
        .collect();
    let min_group = distribute(&packages, 4);
    Ok(min_group.iter().product())
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
