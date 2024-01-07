package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

const (
	TOTAL_RED   = 12
	TOTAL_GREEN = 13
	TOTAL_BLUE  = 14
)

func usage(progname string) {
	log.Fatalf("usage: %s <file>\n", progname)
	os.Exit(1)
}

func process(contents string) uint32 {
	result := uint32(0)
	lines := strings.Split(contents, "\n")
	for _, line := range lines {
		if len(line) == 0 {
			break
		}
		parts := strings.Split(line, ": ")
		gamePart, drawsPart := parts[0], parts[1]
		parts = strings.Split(gamePart, " ")
		gameNumStr := parts[1]
		gameNum, err := strconv.ParseUint(gameNumStr, 10, 32)
		if err != nil {
			panic(err)
		}
		valid := true
		for _, drawPart := range strings.Split(drawsPart, "; ") {
			for _, colorAmount := range strings.Split(drawPart, ", ") {
				parts = strings.Split(colorAmount, " ")
				amountStr, color := parts[0], parts[1]
				amount, err := strconv.ParseUint(amountStr, 10, 32)
				if err != nil {
					panic(err)
				}
				switch color {
				case "red":
					if amount > TOTAL_RED {
						valid = false
					}
				case "green":
					if amount > TOTAL_GREEN {
						valid = false
					}
				case "blue":
					if amount > TOTAL_BLUE {
						valid = false
					}
				default:
					panic("unknown color")
				}
			}
		}
		if valid {
			result += uint32(gameNum)
		}
	}
	return result
}

func main() {
	if len(os.Args) < 2 {
		usage(os.Args[0])
	}
	filename := os.Args[1]
	buf, err := os.ReadFile(filename)
	if err != nil {
		log.Fatal(err)
	}
	contents := string(buf[:])
	result := process(contents)
	fmt.Printf("result = %d\n", result)
}
