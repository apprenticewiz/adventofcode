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
		codeLen := len(line)
		memLen := 0
		i := 1
		for i < len(line)-1 {
			switch line[i] {
			case '\\':
				switch line[i+1] {
				case '\\':
					i += 2
				case '"':
					i += 2
				case 'x':
					i += 4
				default:
					i += 1
				}
			default:
				i += 1
			}
			memLen += 1
		}
		result += uint32(codeLen - memLen)
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
