use std::env;
use std::fs;
use std::io;
use std::process;

use serde_json::Value;

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_file(filename: &str) -> io::Result<Value> {
    let content = fs::read_to_string(filename)?;
    let json: Value = serde_json::from_str(&content).unwrap();
    Ok(json)
}

fn sum_json(value: &Value) -> i64 {
    match value {
        Value::Number(n) => n.as_i64().unwrap_or(0),
        Value::Array(arr) => arr.iter().map(sum_json).sum(),
        Value::Object(obj) => {
            if obj.values().any(|v| *v == Value::String("red".to_string())) {
                0
            } else {
                obj.values().map(sum_json).sum()
            }
        }
        _ => 0,
    }
}

fn process_file(filename: &str) -> io::Result<i64> {
    let json = parse_file(filename)?;
    let result = sum_json(&json);
    Ok(result)
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
