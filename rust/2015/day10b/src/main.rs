use std::env;
use std::io;
use std::process;

use itertools::Itertools;

const NUM_STEPS: usize = 50;

type Rle = Vec<(char, usize)>;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <string>");
    process::exit(1);
}

fn encode(s: &str) -> Rle {
    s.chars()
        .chunk_by(|&c| c)
        .into_iter()
        .map(|(key, group)| (key, group.count()))
        .collect()
}

fn look_and_say(rle: &Rle) -> Rle {
    let expanded: String = rle
        .iter()
        .map(|(ch, count)| format!("{}{}", count, ch))
        .collect();
    encode(&expanded)
}

fn process_input(s: &str) -> io::Result<u32> {
    let mut rle = encode(s);
    for _ in 1..=NUM_STEPS {
        rle = look_and_say(&rle);
    }
    let len = rle.iter().map(|(_, n)| *n).sum::<usize>() as u32;
    Ok(len)
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
