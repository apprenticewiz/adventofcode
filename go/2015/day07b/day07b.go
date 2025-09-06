package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

type Operator int

const (
	Assign Operator = iota
	Not
	And
	Or
	LeftShift
	RightShift
)

type Operation struct {
	operator Operator
	source1 string
	source2 string
	amount int
}

func usage(progname string) {
	fmt.Fprintf(os.Stderr, "usage: %s <input file>\n", progname)
	os.Exit(1)
}

func process(filename string) int {
	operations := make(map[string]Operation)
	cache := make(map[string]int)
	re1 := regexp.MustCompile(`^(\d+|\w+) -> (\w+)$`)
	re2 := regexp.MustCompile(`NOT (\d+|\w+) -> (\w+)$`)
	re3 := regexp.MustCompile(`(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)`)
	re4 := regexp.MustCompile(`(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)`)

	file, err := os.Open(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		matches := re1.FindStringSubmatch(line)
		if matches != nil {
			src := matches[1]
			dest := matches[2]
			operations[dest] = Operation{ Assign, src, "", 0 }
		}
		matches = re2.FindStringSubmatch(line)
		if matches != nil {
			src := matches[1]
			dest := matches[2]
			operations[dest] = Operation{ Not, src, "", 0 }
		}
		matches = re3.FindStringSubmatch(line)
		if matches != nil {
			src1 := matches[1]
			op := matches[2]
			src2 := matches[3]
			dest := matches[4]
			if op == "AND" {
				operations[dest] = Operation{ And, src1, src2, 0 }
			} else {
				operations[dest] = Operation{ Or, src1, src2, 0 }
			}
		}
		matches = re4.FindStringSubmatch(line)
		if matches != nil {
			src := matches[1]
			op := matches[2]
			amt, _ := strconv.Atoi(matches[3])
			dest := matches[4]
			if op == "LSHIFT" {
				operations[dest] = Operation{ LeftShift, src, "", amt }
			} else {
				operations[dest] = Operation{ RightShift, src, "", amt }
			}
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}
	a := eval(operations, cache, "a")
	operations["b"] = Operation{ Assign, strconv.Itoa(a), "", 0 }
	cache = make(map[string]int)
	return eval(operations, cache, "a")
}

func eval(ops map[string]Operation, cache map[string]int, expr string) int {
	n1, err := strconv.Atoi(expr)
	if err == nil {
		return n1
	}
	n2, exists := cache[expr]
	if exists {
		return n2
	}
	op := ops[expr]
	r := 0
	switch op.operator {
	case Assign:
		src := op.source1
		a := eval(ops, cache, src)
		r = a
	case Not:
		src := op.source1
		a := eval(ops, cache, src)
		r = ^a
	case And:
		src1 := op.source1
		src2 := op.source2
		a := eval(ops, cache, src1)
		b := eval(ops, cache, src2)
		r = a & b
	case Or:
		src1 := op.source1
		src2 := op.source2
		a := eval(ops, cache, src1)
		b := eval(ops, cache, src2)
		r = a | b
	case LeftShift:
		src := op.source1
		amt := op.amount
		a := eval(ops, cache, src)
		r = a << amt
	case RightShift:
		src := op.source1
		amt := op.amount
		a := eval(ops, cache, src)
		r = a >> amt
	}
	masked := r & 0xffff
	cache[expr] = masked
	return masked
}

func main() {
	if len(os.Args) < 2 {
		usage(os.Args[0])
	}

	filename := os.Args[1]
	result := process(filename)
	fmt.Printf("result = %d\n", result)
}
