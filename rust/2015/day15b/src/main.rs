use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use regex::Regex;

struct Ingredient {
    capacity: i32,
    durability: i32,
    flavor: i32,
    texture: i32,
    calories: i32,
}

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_file(filename: &str) -> io::Result<Vec<Ingredient>> {
    let mut ingredients: Vec<Ingredient> = vec![];
    let re = Regex::new(
        r"\w+: capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)"
    )
    .unwrap();
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    for line_result in reader.lines() {
        let line = line_result?;
        if let Some(caps) = re.captures(&line) {
            let capacity = caps.get(1).unwrap().as_str().parse::<i32>().unwrap_or(0);
            let durability = caps.get(2).unwrap().as_str().parse::<i32>().unwrap_or(0);
            let flavor = caps.get(3).unwrap().as_str().parse::<i32>().unwrap_or(0);
            let texture = caps.get(4).unwrap().as_str().parse::<i32>().unwrap_or(0);
            let calories = caps.get(5).unwrap().as_str().parse::<i32>().unwrap_or(0);
            ingredients.push(Ingredient {
                capacity,
                durability,
                flavor,
                texture,
                calories,
            });
        }
    }
    Ok(ingredients)
}

fn distribute(total: i32, parts: usize) -> Vec<Vec<i32>> {
    if total == 0 {
        vec![vec![0; parts]]
    } else if parts == 1 {
        vec![vec![total]]
    } else {
        let mut result: Vec<Vec<i32>> = vec![];
        for x in 0..=total {
            let sub_dists = distribute(total - x, parts - 1);
            for mut sub_dist in sub_dists {
                let mut combo = vec![x];
                combo.append(&mut sub_dist);
                result.push(combo);
            }
        }
        result
    }
}

fn total_calories(ingredients: &[Ingredient], allocation: &[i32]) -> i32 {
    let mut total = 0;
    for (ingredient, &amt) in ingredients.iter().zip(allocation.iter()) {
        total += ingredient.calories * amt;
    }
    total
}

fn score(ingredients: &[Ingredient], allocation: &[i32]) -> i32 {
    let mut totals = [0; 4];
    for (ingredient, &amt) in ingredients.iter().zip(allocation.iter()) {
        totals[0] += ingredient.capacity * amt;
        totals[1] += ingredient.durability * amt;
        totals[2] += ingredient.flavor * amt;
        totals[3] += ingredient.texture * amt;
    }
    for total in totals.iter_mut() {
        *total = *total.max(&mut 0);
    }
    totals.iter().product()
}

fn max_score(ingredients: &[Ingredient]) -> i32 {
    let allocations = distribute(100, ingredients.len());
    allocations
        .iter()
        .filter(|x| total_calories(ingredients, x) == 500)
        .map(|x| score(ingredients, x))
        .max()
        .unwrap()
}

fn process_file(filename: &str) -> io::Result<i32> {
    let ingredients = parse_file(filename)?;
    Ok(max_score(&ingredients))
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
