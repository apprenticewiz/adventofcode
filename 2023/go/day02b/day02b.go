package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
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
		drawsPart := parts[1]
		redNeeded, greenNeeded, blueNeeded := uint32(0), uint32(0), uint32(0)
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
					if uint32(amount) > redNeeded {
						redNeeded = uint32(amount)
					}
				case "green":
					if uint32(amount) > greenNeeded {
						greenNeeded = uint32(amount)
					}
				case "blue":
					if uint32(amount) > blueNeeded {
						blueNeeded = uint32(amount)
					}
				default:
					panic("unknown color")
				}
			}
		}
		result += redNeeded * greenNeeded * blueNeeded
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
