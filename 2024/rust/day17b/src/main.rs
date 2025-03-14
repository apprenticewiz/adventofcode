use std::collections::VecDeque;
use std::env;
use std::fmt;
use std::fs;
use std::process;
use std::time::SystemTime;

struct Computer {
    reg_a: u64,
    reg_b: u64,
    reg_c: u64,
    ip: u64,
    output: Vec<u64>,
}

impl Computer {
    fn new(reg_a: u64, reg_b: u64, reg_c: u64) -> Computer {
        Computer {
            reg_a,
            reg_b,
            reg_c,
            ip: 0,
            output: vec![],
        }
    }

    fn combo(&self, operand: u64) -> u64 {
        match operand {
            0..=3 => operand,
            4 => self.reg_a,
            5 => self.reg_b,
            6 => self.reg_c,
            _ => panic!("invalid operand"),
        }
    }

    fn adv(&mut self, operand: u64) {
        let numerator: u64 = self.reg_a;
        let denominator: u64 = 2_u64.pow(self.combo(operand).try_into().unwrap());
        self.reg_a = numerator / denominator;
        self.ip += 2;
    }

    fn bxl(&mut self, operand: u64) {
        self.reg_b ^= operand;
        self.ip += 2;
    }

    fn bst(&mut self, operand: u64) {
        self.reg_b = self.combo(operand) % 8;
        self.ip += 2;
    }

    fn jnz(&mut self, operand: u64) {
        if self.reg_a == 0 {
            self.ip += 2;
        } else {
            self.ip = operand;
        }
    }

    fn bxc(&mut self, _operand: u64) {
        self.reg_b ^= self.reg_c;
        self.ip += 2;
    }

    fn out(&mut self, operand: u64) {
        self.output.push(self.combo(operand) % 8);
        self.ip += 2;
    }

    fn bdv(&mut self, operand: u64) {
        let numerator: u64 = self.reg_a;
        let denominator: u64 = 2_u64.pow(self.combo(operand).try_into().unwrap());
        self.reg_b = numerator / denominator;
        self.ip += 2;
    }

    fn cdv(&mut self, operand: u64) {
        let numerator: u64 = self.reg_a;
        let denominator: u64 = 2_u64.pow(self.combo(operand).try_into().unwrap());
        self.reg_c = numerator / denominator;
        self.ip += 2;
    }

    fn dispatch(&mut self, opcode: u64, operand: u64) {
        match opcode {
            0 => self.adv(operand),
            1 => self.bxl(operand),
            2 => self.bst(operand),
            3 => self.jnz(operand),
            4 => self.bxc(operand),
            5 => self.out(operand),
            6 => self.bdv(operand),
            7 => self.cdv(operand),
            _ => panic!("invalid opcode"),
        }
    }

    fn execute(&mut self, program: &[u64], once: bool) {
        while (self.ip as usize) < program.len() {
            let opcode = program[self.ip as usize];
            let operand = program[self.ip as usize + 1];
            if opcode == 3 && once {
                break;
            }
            self.dispatch(opcode, operand);
        }
    }
}

impl fmt::Display for Computer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s: String = String::new();
        for i in 0..self.output.len() {
            s.push_str(format!("{}", self.output[i]).as_str());
            if i < self.output.len() - 1 {
                s.push(',');
            }
        }
        write!(f, "{}", s)
    }
}

type Program = Vec<u64>;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn scan_input(contents: &str) -> (Computer, Program) {
    let mut reg_a: u64 = 0;
    let mut reg_b: u64 = 0;
    let mut reg_c: u64 = 0;
    let mut program: Vec<u64> = vec![];
    for line in contents.lines() {
        if line.starts_with("Register A:") {
            let (_, val) = line.split_once(':').unwrap();
            reg_a = val.trim().parse::<u64>().unwrap();
        } else if line.starts_with("Register B:") {
            let (_, val) = line.split_once(':').unwrap();
            reg_b = val.trim().parse::<u64>().unwrap();
        } else if line.starts_with("Register C:") {
            let (_, val) = line.split_once(':').unwrap();
            reg_c = val.trim().parse::<u64>().unwrap();
        } else if line.starts_with("Program:") {
            let (_, val) = line.split_once(':').unwrap();
            program = val
                .trim()
                .split(',')
                .map(|x| x.parse::<u64>().unwrap())
                .collect::<Vec<u64>>();
        }
    }
    (Computer::new(reg_a, reg_b, reg_c), program)
}

fn solve(program: &Program) -> u64 {
    let values = program.clone();
    let mut results: Vec<u64> = vec![];
    let mut q: VecDeque<(u64, u64)> = VecDeque::new();
    q.push_back((0, 1));
    while let Some((a, n)) = q.pop_front() {
        let value = &values[values.len() - n as usize];
        for i in 0..=7 {
            let mut comp = Computer::new(a + i as u64, 0, 0);
            comp.execute(program, true);
            if &comp.output[0] == value {
                if n == values.len() as u64 {
                    results.push(a + i as u64);
                } else {
                    q.push_back(((a + i as u64) * 8, n + 1));
                }
            }
        }
    }
    *results.iter().min().unwrap()
}

fn process(contents: &str) -> u64 {
    let (_, program) = scan_input(contents);
    solve(&program)
}

fn calc_runtime(start_time: &SystemTime) -> String {
    let mut elapsed = start_time.elapsed().unwrap().as_micros() as f64;
    let mut units = "μs";
    if elapsed > 1000.0 {
        elapsed /= 1000.0;
        units = "ms";
    }
    if elapsed > 1000.0 {
        elapsed /= 1000.0;
        units = "s";
    }
    format!("(elapsed time: {elapsed} {units})")
}

fn main() {
    let start_time = SystemTime::now();
    if env::args().count() < 2 {
        usage();
    }
    let filename = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(filename).expect("read of input file failed");
    let result = process(&contents);
    let duration = calc_runtime(&start_time);
    println!("result = {result}  {duration}");
}
