package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func usage(progname string) {
	fmt.Fprintf(os.Stderr, "usage: %s <input file>\n", progname)
	os.Exit(1)
}

func prop1(str string) bool {
	vowels := 0
	for _, ch := range str {
		if ch == 'a' || ch == 'e' || ch == 'i' || ch == 'o' || ch == 'u' {
			vowels++
		}
	}
	return vowels >= 3
}

func prop2(str string) bool {
	for i := 0; i < len(str)-1; i++ {
		if str[i] == str[i+1] {
			return true
		}
	}
	return false
}

func prop3(str string) bool {
	return !strings.Contains(str, "ab") &&
		!strings.Contains(str, "cd") &&
		!strings.Contains(str, "pq") &&
		!strings.Contains(str, "xy")
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
		if prop1(line) && prop2(line) && prop3(line) {
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
