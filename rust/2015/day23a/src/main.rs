use std::env;
use std::fs;
use std::io;
use std::process;

use regex::Regex;

#[derive(Clone, Debug, Eq, PartialEq)]
enum Register {
    A,
    B,
}

impl Register {
    fn from(s: &str) -> Register {
        match s {
            "a" => Register::A,
            "b" => Register::B,
            _ => panic!("invalid register"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Instruction {
    Half(Register),
    Triple(Register),
    Increment(Register),
    Jump(i32),
    JumpIfEven(Register, i32),
    JumpIfOne(Register, i32),
}

type Program = Vec<Instruction>;

#[derive(Clone, Debug, Eq, PartialEq)]
struct Cpu {
    a: i32,
    b: i32,
    ip: i32,
}

impl Cpu {
    fn new() -> Cpu {
        Cpu { a: 0, b: 0, ip: 0 }
    }

    fn execute(&mut self, program: &Program) {
        loop {
            if self.ip < 0 || self.ip >= (program.len() as i32) {
                break;
            }
            match &program[self.ip as usize] {
                Instruction::Half(reg) => {
                    match reg {
                        Register::A => self.a /= 2,
                        Register::B => self.b /= 2,
                    }
                    self.ip += 1;
                }
                Instruction::Triple(reg) => {
                    match reg {
                        Register::A => self.a *= 3,
                        Register::B => self.b *= 3,
                    }
                    self.ip += 1;
                }
                Instruction::Increment(reg) => {
                    match reg {
                        Register::A => self.a += 1,
                        Register::B => self.b += 1,
                    }
                    self.ip += 1;
                }
                Instruction::Jump(offset) => self.ip += offset,
                Instruction::JumpIfEven(reg, offset) => {
                    self.ip += match reg {
                        Register::A => {
                            if self.a % 2 == 0 {
                                *offset
                            } else {
                                1
                            }
                        }
                        Register::B => {
                            if self.b % 2 == 0 {
                                *offset
                            } else {
                                1
                            }
                        }
                    }
                }
                Instruction::JumpIfOne(reg, offset) => {
                    self.ip += match reg {
                        Register::A => {
                            if self.a == 1 {
                                *offset
                            } else {
                                1
                            }
                        }
                        Register::B => {
                            if self.b == 1 {
                                *offset
                            } else {
                                1
                            }
                        }
                    }
                }
            }
        }
    }
}

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn parse_file(filename: &str) -> io::Result<Program> {
    let mut program: Program = vec![];
    let re =
        Regex::new(r"((hlf|tpl|inc) (a|b))|(jmp ([+-]\d+))|((jie|jio) (a|b), ([+-]\d+))").unwrap();
    let content = fs::read_to_string(filename)?;
    for line in content.lines() {
        if let Some(caps) = re.captures(line) {
            if caps.get(1).is_some() {
                let instr = caps.get(2).unwrap().as_str();
                let operand = Register::from(caps.get(3).unwrap().as_str());
                match instr {
                    "hlf" => program.push(Instruction::Half(operand)),
                    "tpl" => program.push(Instruction::Triple(operand)),
                    "inc" => program.push(Instruction::Increment(operand)),
                    _ => unreachable!(),
                }
            } else if caps.get(4).is_some() {
                let offset = caps.get(5).unwrap().as_str().parse::<i32>().unwrap();
                program.push(Instruction::Jump(offset));
            } else if caps.get(6).is_some() {
                let instr = caps.get(7).unwrap().as_str();
                let operand = Register::from(caps.get(8).unwrap().as_str());
                let offset = caps.get(9).unwrap().as_str().parse::<i32>().unwrap();
                match instr {
                    "jie" => program.push(Instruction::JumpIfEven(operand, offset)),
                    "jio" => program.push(Instruction::JumpIfOne(operand, offset)),
                    _ => unreachable!(),
                }
            }
        } else {
            panic!("malformed input");
        }
    }
    Ok(program)
}

fn process_file(filename: &str) -> io::Result<i32> {
    let program = parse_file(filename)?;
    let mut cpu = Cpu::new();
    cpu.execute(&program);
    Ok(cpu.b)
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
