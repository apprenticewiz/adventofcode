use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::{Command, Stdio};

type Button = Vec<usize>;
type Joltage = i64;

#[derive(Debug, Clone)]
struct Machine {
    goals: Vec<Joltage>,
    buttons: Vec<Button>,
}

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    std::process::exit(1);
}

fn parse_lights(s: &str) -> Option<String> {
    let s = s.strip_prefix('[')?.strip_suffix(']')?;
    Some(s.to_string())
}

fn parse_button(s: &str) -> Option<Button> {
    let s = s.strip_prefix('(')?.strip_suffix(')')?;
    s.split(',')
        .map(|num_str| num_str.parse::<usize>().ok())
        .collect()
}

fn parse_joltage(s: &str) -> Option<Vec<Joltage>> {
    let s = s.strip_prefix('{')?.strip_suffix('}')?;
    s.split(',')
        .map(|num_str| num_str.parse::<Joltage>().ok())
        .collect()
}

fn parse_line(line: &str) -> Option<Machine> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.is_empty() {
        return None;
    }
    parse_lights(parts[0])?;
    let mut buttons = Vec::new();
    let mut i = 1;
    while i < parts.len() && parts[i].starts_with('(') {
        buttons.push(parse_button(parts[i])?);
        i += 1;
    }
    let goals = if i < parts.len() {
        parse_joltage(parts[i])?
    } else {
        Vec::new()
    };
    Some(Machine { goals, buttons })
}

fn generate_smt(goals: &[Joltage], buttons: &[Button]) -> String {
    let num_buttons = buttons.len();
    let mut smt = String::new();
    smt.push_str("(set-logic LIA)\n");
    smt.push_str("(set-option :produce-models true)\n");
    for i in 0..num_buttons {
        smt.push_str(&format!("(declare-const x{} Int)\n", i));
    }
    for i in 0..num_buttons {
        smt.push_str(&format!("(assert (>= x{} 0))\n", i));
    }
    for (counter_idx, current_goal) in goals.iter().enumerate() {
        let affecting_buttons: Vec<usize> = buttons
            .iter()
            .enumerate()
            .filter(|(_, button)| button.contains(&counter_idx))
            .map(|(j, _)| j)
            .collect();
        let sum = if affecting_buttons.is_empty() {
            "0".to_string()
        } else if affecting_buttons.len() == 1 {
            format!("x{}", affecting_buttons[0])
        } else {
            let terms: Vec<String> = affecting_buttons
                .iter()
                .map(|&j| format!("x{}", j))
                .collect();
            format!("(+ {})", terms.join(" "))
        };
        smt.push_str(&format!("(assert (= {} {}))\n", sum, current_goal));
    }
    let objective_terms: Vec<String> = (0..num_buttons).map(|i| format!("x{}", i)).collect();
    smt.push_str(&format!("(minimize (+ {}))\n", objective_terms.join(" ")));
    smt.push_str("(check-sat)\n");
    smt.push_str("(get-objectives)\n");
    smt.push_str("(exit)\n");
    smt
}

fn parse_model(output: &str) -> Option<i64> {
    let all_words: Vec<&str> = output.split_whitespace().collect();
    let numbers: Vec<i64> = all_words
        .iter()
        .filter_map(|w| {
            let digits: String = w.chars().filter(|c| c.is_ascii_digit()).collect();
            if !digits.is_empty() && w.chars().all(|c| c.is_ascii_digit() || "()".contains(c)) {
                digits.parse::<i64>().ok()
            } else {
                None
            }
        })
        .collect();
    numbers.last().copied()
}

fn min_presses(machine: &Machine) -> io::Result<i64> {
    let smt_lib = generate_smt(&machine.goals, &machine.buttons);
    let mut child = Command::new("z3")
        .arg("-in")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(smt_lib.as_bytes())?;
    }
    let output = child.wait_with_output()?;
    let output_str = String::from_utf8_lossy(&output.stdout);
    parse_model(&output_str).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Could not parse objective from: {}", output_str),
        )
    })
}

fn process_file(filename: &str) -> io::Result<i64> {
    let contents = fs::read_to_string(filename)?;
    let machines: Vec<Machine> = contents
        .lines()
        .filter(|line| !line.is_empty())
        .filter_map(parse_line)
        .collect();
    let mut total = 0i64;
    for machine in &machines {
        total += min_presses(machine)?;
    }
    Ok(total)
}

fn display_duration(duration: u128) {
    print!("elapsed time: ");
    let duration_f = duration as f64;
    if duration_f < 1_000.0 {
        println!("{} ns", duration_f);
    } else if duration_f < 1_000_000.0 {
        println!("{} μs", duration_f / 1_000.0);
    } else if duration_f < 1_000_000_000.0 {
        println!("{} ms", duration_f / 1_000_000.0);
    } else {
        println!("{} s", duration_f / 1_000_000_000.0);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let filename = &args[1];
    let start = std::time::Instant::now();
    match process_file(filename) {
        Ok(result) => {
            let duration = start.elapsed().as_nanos();
            println!("result = {result}");
            display_duration(duration);
        }
        Err(e) => {
            eprintln!("error while processing file `{filename}': {e}");
            std::process::exit(1);
        }
    }
}
