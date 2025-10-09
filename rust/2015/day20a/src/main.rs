use std::env;
use std::io;
use std::process;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <num>");
    process::exit(1);
}

fn process_input(s: &str) -> io::Result<u32> {
    let n = s.parse::<u32>().expect("input must be a number!");
    let limit = n / 10;
    let mut houses: Vec<u32> = vec![0; (limit + 1) as usize];
    for elf in 1..=limit {
        let gift = elf * 10;
        for house in (elf..=limit).step_by(elf as usize) {
            houses[house as usize] += gift;
        }
    }
    let mut i = 1;
    loop {
        let p = houses[i];
        if p >= n {
            return Ok(i as u32);
        } else {
            i += 1;
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let s = &args[1];
    match process_input(s) {
        Ok(result) => println!("result = {result}"),
        Err(e) => {
            eprintln!("error while processing input: {e}");
            process::exit(1);
        }
    }
}
