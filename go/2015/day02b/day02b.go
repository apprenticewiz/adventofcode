package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func usage(progname string) {
	fmt.Fprintf(os.Stderr, "usage: %s <input file>\n", progname)
	os.Exit(1)
}

func process(filename string) uint32 {
	var totalLen uint32 = 0

	file, err := os.Open(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, "x")
		if len(parts) != 3 {
			fmt.Fprintf(os.Stderr, "Invalid line format: %s\n", line)
			os.Exit(1)
		}
		dimensions := make([]uint32, 3)
		for i, part := range parts {
			num, err := strconv.ParseUint(part, 10, 32)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Invalid number in line: %s", line)
				os.Exit(1)
			}
			dimensions[i] = uint32(num)
		}
		l, w, h := dimensions[0], dimensions[1], dimensions[2]
		perim1 := 2 * (l + w)
		perim2 := 2 * (l + h)
		perim3 := 2 * (w + h)
		presentLen := perim1
		if perim2 < presentLen {
			presentLen = perim2
		}
		if perim3 < presentLen {
			presentLen = perim3
		}
		bowLen := l * w * h
		totalLen += presentLen + bowLen
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	return totalLen
}

func main() {
	if len(os.Args) < 2 {
		usage(os.Args[0])
	}

	filename := os.Args[1]
	result := process(filename)
	fmt.Printf("result = %d\n", result)
}
