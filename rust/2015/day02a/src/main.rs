use std::cmp::min;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn process_file(filename: &str) -> io::Result<u32> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut total_area: u32 = 0;
    for line_result in reader.lines() {
        let line = line_result?;
        let dimensions = line
            .split('x')
            .map(|x| x.parse().unwrap())
            .collect::<Vec<u32>>();
        let area1 = dimensions[0] * dimensions[1];
        let area2 = dimensions[0] * dimensions[2];
        let area3 = dimensions[1] * dimensions[2];
        let surface_area = (area1 << 1) + (area2 << 1) + (area3 << 1);
        let min_area = min(min(area1, area2), area3);
        total_area += surface_area + min_area
    }
    Ok(total_area)
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
