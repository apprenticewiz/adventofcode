use std::env;
use std::fs;
use std::io;
use std::process;

use regex::Regex;

type Element = String;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_file(filename: &str) -> io::Result<Vec<Element>> {
    let content = fs::read_to_string(filename)?;
    let last_line = content.lines().last().unwrap();
    let re = Regex::new(r"([A-Z][a-z]*)").unwrap();
    let mut elements: Vec<Element> = vec![];
    for (_, [element]) in re.captures_iter(last_line).map(|c| c.extract()) {
        elements.push(element.to_string());
    }
    Ok(elements)
}

fn process_file(filename: &str) -> io::Result<u32> {
    let elements = parse_file(filename)?;
    let result = elements.len()
        - elements
            .iter()
            .filter(|x| x.as_str() == "Rn" || x.as_str() == "Ar")
            .count()
        - (2 * elements.iter().filter(|x| x.as_str() == "Y").count())
        - 1;
    Ok(result as u32)
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
