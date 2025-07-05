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
    let mut total_len: u32 = 0;
    for line_result in reader.lines() {
        let line = line_result?;
        let dimensions = line
            .split('x')
            .map(|x| x.parse().unwrap())
            .collect::<Vec<u32>>();
        let perim1 = (dimensions[0] + dimensions[1]) << 1;
        let perim2 = (dimensions[0] + dimensions[2]) << 1;
        let perim3 = (dimensions[1] + dimensions[2]) << 1;
        let present_len = min(min(perim1, perim2), perim3);
        let bow_len = dimensions[0] * dimensions[1] * dimensions[2];
        total_len += present_len + bow_len;
    }
    Ok(total_len)
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
