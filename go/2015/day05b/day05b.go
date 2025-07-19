package main

import (
	"bufio"
	"fmt"
	"os"
)

func usage(progname string) {
	fmt.Fprintf(os.Stderr, "usage: %s <input file>\n", progname)
	os.Exit(1)
}

func prop1(str string) bool {
	for outerStart := 0; outerStart < len(str)-3; outerStart++ {
		firstPair := str[outerStart : outerStart+2]
		for innerStart := outerStart + 2; innerStart < len(str)-1; innerStart++ {
			secondPair := str[innerStart : innerStart+2]
			if firstPair == secondPair {
				return true
			}
		}
	}
	return false
}

func prop2(str string) bool {
	for i := 0; i < len(str)-2; i++ {
		if str[i] == str[i+2] {
			return true
		}
	}
	return false
}

func process(filename string) uint32 {
	var result uint32 = 0

	file, err := os.Open(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if prop1(line) && prop2(line) {
			result++
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	return result
}

func main() {
	if len(os.Args) < 2 {
		usage(os.Args[0])
	}

	filename := os.Args[1]
	result := process(filename)
	fmt.Printf("result = %d\n", result)
}
