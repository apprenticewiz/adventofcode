use regex::Regex;
use std::collections::{HashMap, VecDeque};
use std::env;
use std::fs;
use std::process;

#[derive(Clone, Debug)]
enum Predicate {
    LessThan(String, u32),
    GreaterThan(String, u32),
    Unconditional,
}

#[derive(Clone, Debug, PartialEq)]
enum Action {
    Accept,
    Reject,
    Goto(String),
}

type Rule = (Predicate, Action);

type Workflow = Vec<Rule>;

#[derive(Clone, Copy, Debug)]
struct Interval {
    from: u32,
    to: u32,
}

#[derive(Clone, Debug)]
struct State {
    label: String,
    x: Interval,
    m: Interval,
    a: Interval,
    s: Interval,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn parse_input(contents: &str) -> HashMap<String, Workflow> {
    let mut flows: HashMap<String, Workflow> = HashMap::new();
    let mut lineptr = contents.lines();
    let flow_re = Regex::new(r"([a-z]+)\{([A-Za-z0-9:<>,]+)\}").unwrap();
    let cond_re = Regex::new(r"([xmas])(<|>)(\d+):(A|R|[a-z]+)").unwrap();
    loop {
        let line = lineptr.next().unwrap();
        if line.is_empty() {
            break;
        }
        let flow_caps = flow_re.captures(line).unwrap();
        let flow_id = flow_caps.get(1).unwrap().as_str();
        let rules_str = flow_caps.get(2).unwrap().as_str();
        let mut workflow: Workflow = vec![];
        for rule in rules_str.split(',') {
            if cond_re.is_match(rule) {
                let cond_caps = cond_re.captures(rule).unwrap();
                let rating = cond_caps.get(1).unwrap().as_str();
                let test = cond_caps.get(2).unwrap().as_str();
                let value = cond_caps.get(3).unwrap().as_str().parse::<u32>().unwrap();
                let action_str = cond_caps.get(4).unwrap().as_str();
                let predicate = match test {
                    "<" => Predicate::LessThan(rating.to_string(), value),
                    ">" => Predicate::GreaterThan(rating.to_string(), value),
                    _ => unreachable!(),
                };
                let action = match action_str {
                    "A" => Action::Accept,
                    "R" => Action::Reject,
                    _ => Action::Goto(action_str.to_string()),
                };
                let rule = (predicate, action);
                workflow.push(rule);
            } else {
                let predicate = Predicate::Unconditional;
                let action = match rule {
                    "A" => Action::Accept,
                    "R" => Action::Reject,
                    _ => Action::Goto(rule.to_string()),
                };
                let rule = (predicate, action);
                workflow.push(rule);
            }
        }
        flows.insert(flow_id.to_string(), workflow);
    }
    flows
}

fn scan_ranges(flows: &HashMap<String, Workflow>) -> u64 {
    let init_state = State {
        label: String::from("in"),
        x: Interval { from: 1, to: 4000 },
        m: Interval { from: 1, to: 4000 },
        a: Interval { from: 1, to: 4000 },
        s: Interval { from: 1, to: 4000 },
    };
    let mut q: VecDeque<State> = VecDeque::new();
    let mut accepted: Vec<State> = vec![];
    q.push_back(init_state);
    while !q.is_empty() {
        let state = q.pop_front().unwrap();
        let rules = flows.get(&state.label).unwrap();
        let mut working_state = state.clone();
        for rule in rules.iter() {
            match rule {
                (Predicate::Unconditional, Action::Reject) => {
                    // reject this state, so do nothing
                }
                (Predicate::Unconditional, Action::Accept) => {
                    // add this state to the accepted list
                    accepted.push(working_state.clone());
                }
                (Predicate::Unconditional, Action::Goto(label)) => {
                    // test the current working state on the next node
                    working_state.label = label.clone();
                    q.push_back(working_state.clone());
                }
                (Predicate::LessThan(rating, amount), Action::Reject) => {
                    // split the range and continue with the non-rejected part
                    match rating.as_str() {
                        "x" => working_state.x.from = *amount,
                        "m" => working_state.m.from = *amount,
                        "a" => working_state.a.from = *amount,
                        "s" => working_state.s.from = *amount,
                        _ => unreachable!(),
                    }
                }
                (Predicate::LessThan(rating, amount), Action::Accept) => {
                    // split the range, accept the less than part, continue
                    let mut accepted_state = working_state.clone();
                    match rating.as_str() {
                        "x" => {
                            accepted_state.x.to = *amount - 1;
                            working_state.x.from = *amount;
                        }
                        "m" => {
                            accepted_state.m.to = *amount - 1;
                            working_state.m.from = *amount;
                        }
                        "a" => {
                            accepted_state.a.to = *amount - 1;
                            working_state.a.from = *amount;
                        }
                        "s" => {
                            accepted_state.s.to = *amount - 1;
                            working_state.s.from = *amount;
                        }
                        _ => unreachable!(),
                    }
                    accepted.push(accepted_state);
                }
                (Predicate::LessThan(rating, amount), Action::Goto(label)) => {
                    //split the range, test on the less than part at the new node, continue
                    let mut goto_state = working_state.clone();
                    match rating.as_str() {
                        "x" => {
                            goto_state.x.to = *amount - 1;
                            working_state.x.from = *amount;
                        }
                        "m" => {
                            goto_state.m.to = *amount - 1;
                            working_state.m.from = *amount;
                        }
                        "a" => {
                            goto_state.a.to = *amount - 1;
                            working_state.a.from = *amount;
                        }
                        "s" => {
                            goto_state.s.to = *amount - 1;
                            working_state.s.from = *amount;
                        }
                        _ => unreachable!(),
                    }
                    goto_state.label = label.clone();
                    q.push_back(goto_state);
                }
                (Predicate::GreaterThan(rating, amount), Action::Reject) => {
                    // split the range and continue with the non-rejected part
                    match rating.as_str() {
                        "x" => working_state.x.to = *amount,
                        "m" => working_state.m.to = *amount,
                        "a" => working_state.a.to = *amount,
                        "s" => working_state.s.to = *amount,
                        _ => unreachable!(),
                    }
                }
                (Predicate::GreaterThan(rating, amount), Action::Accept) => {
                    // split the range, accept the greater than part, continue
                    let mut accepted_state = working_state.clone();
                    match rating.as_str() {
                        "x" => {
                            accepted_state.x.from = *amount + 1;
                            working_state.x.to = *amount;
                        }
                        "m" => {
                            accepted_state.m.from = *amount + 1;
                            working_state.m.to = *amount;
                        }
                        "a" => {
                            accepted_state.a.from = *amount + 1;
                            working_state.a.to = *amount;
                        }
                        "s" => {
                            accepted_state.s.from = *amount + 1;
                            working_state.s.to = *amount;
                        }
                        _ => unreachable!(),
                    }
                    accepted.push(accepted_state);
                }
                (Predicate::GreaterThan(rating, amount), Action::Goto(label)) => {
                    // split the range, test on the greater than part at the new node, continue
                    let mut goto_state = working_state.clone();
                    match rating.as_str() {
                        "x" => {
                            goto_state.x.from = *amount + 1;
                            working_state.x.to = *amount;
                        }
                        "m" => {
                            goto_state.m.from = *amount + 1;
                            working_state.m.to = *amount;
                        }
                        "a" => {
                            goto_state.a.from = *amount + 1;
                            working_state.a.to = *amount;
                        }
                        "s" => {
                            goto_state.s.from = *amount + 1;
                            working_state.s.to = *amount;
                        }
                        _ => unreachable!(),
                    }
                    goto_state.label = label.clone();
                    q.push_back(goto_state);
                }
            }
        }
    }
    let mut result: u64 = 0;
    for state in accepted.iter() {
        result += (state.x.to - state.x.from + 1) as u64
            * (state.m.to - state.m.from + 1) as u64
            * (state.a.to - state.a.from + 1) as u64
            * (state.s.to - state.s.from + 1) as u64;
    }
    result
}

fn process(contents: &str) -> u64 {
    let flows = parse_input(contents);
    scan_ranges(&flows)
}

fn main() {
    if env::args().count() < 2 {
        usage();
    }
    let filename = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(filename).expect("read of input file failed");
    let result = process(&contents);
    println!("result = {result}");
}
