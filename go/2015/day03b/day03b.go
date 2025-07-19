package main

import (
	"bufio"
	"fmt"
	"os"
)

type Position2D struct {
	X int32
	Y int32
}

func usage(progname string) {
	fmt.Fprintf(os.Stderr, "usage: %s <input file>\n", progname)
	os.Exit(1)
}

func process(filename string) uint32 {
	file, err := os.Open(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	santa := Position2D{ 0, 0 };
	roboSanta := Position2D{ 0, 0 };
	positions := make(map[Position2D]struct{})
	positions[santa] = struct{}{}

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		i := 0
		for _, ch := range line {
			if i % 2 == 0 {
				switch ch {
				case '^':
					santa.Y += 1
				case 'v':
					santa.Y -= 1
				case '<':
					santa.X -= 1
				case '>':
					santa.X += 1
				}
				positions[santa] = struct{}{}
			} else {
				switch ch {
				case '^':
					roboSanta.Y += 1
				case 'v':
					roboSanta.Y -= 1
				case '<':
					roboSanta.X -= 1
				case '>':
					roboSanta.X += 1
				}
				positions[roboSanta] = struct{}{}
			}
			i++
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	return uint32(len(positions))
}

func main() {
	if len(os.Args) < 2 {
		usage(os.Args[0])
	}

	filename := os.Args[1]
	result := process(filename)
	fmt.Printf("result = %d\n", result)
}
