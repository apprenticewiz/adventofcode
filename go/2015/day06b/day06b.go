package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

const (
	MAX_ROWS = 1000
	MAX_COLS = 1000
)

func usage(progname string) {
	fmt.Fprintf(os.Stderr, "usage: %s <input file>\n", progname)
	os.Exit(1)
}

func perform(grid *[MAX_ROWS][MAX_COLS]int, action string, r1 int, c1 int, r2 int, c2 int) {
	for row := r1; row <= r2; row++ {
		for col := c1; col <= c2; col++ {
			switch action {
			case "turn on":
				grid[row][col] += 1
			case "turn off":
				if grid[row][col] > 0 {
					grid[row][col] -= 1
				}
			case "toggle":
				grid[row][col] += 2
			}
		}
	}
}

func sum(grid [MAX_ROWS][MAX_COLS]int) uint32 {
	sum := uint32(0)
	for row := 0; row < MAX_ROWS; row++ {
		for col := 0; col < MAX_COLS; col++ {
			sum += uint32(grid[row][col])
		}
	}
	return sum
}

func process(filename string) uint32 {
	var grid [MAX_ROWS][MAX_COLS]int
	re := regexp.MustCompile(`(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)`)

	file, err := os.Open(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		matches := re.FindStringSubmatch(line)
		if matches != nil {
			action := matches[1]
			r1, _ := strconv.Atoi(matches[2])
			c1, _ := strconv.Atoi(matches[3])
			r2, _ := strconv.Atoi(matches[4])
			c2, _ := strconv.Atoi(matches[5])
			perform(&grid, action, r1, c1, r2, c2)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	return sum(grid)
}

func main() {
	if len(os.Args) < 2 {
		usage(os.Args[0])
	}

	filename := os.Args[1]
	result := process(filename)
	fmt.Printf("result = %d\n", result)
}
