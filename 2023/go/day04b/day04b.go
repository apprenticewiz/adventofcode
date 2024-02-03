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
	instances := make(map[uint32]uint32)
	lines := strings.Split(contents, "\n")
	for _, line := range lines {
		if len(line) == 0 {
			break
		}
		parts := strings.Split(line, ": ")
		cardPart, rest := parts[0], parts[1]
		cardParts := strings.Split(cardPart, " ")
		cardNum, _ := strconv.ParseUint(cardParts[len(cardParts)-1], 10, 32)
		cardNumber := uint32(cardNum)
		restSplit := strings.Split(rest, " | ")
		winningStr, handStr := restSplit[0], restSplit[1]
		winningSet := make(map[uint32]bool)
		winningNums := strings.Split(winningStr, " ")
		for _, numStr := range winningNums {
			if len(numStr) != 0 {
				num, err := strconv.ParseUint(numStr, 10, 32)
				if err != nil {
					panic(err)
				}
				winningSet[uint32(num)] = true
			}
		}
		handNums := strings.Split(handStr, " ")
		handSet := make(map[uint32]bool)
		for _, numStr := range handNums {
			if len(numStr) != 0 {
				num, err := strconv.ParseUint(numStr, 10, 32)
				if err != nil {
					panic(err)
				}
				handSet[uint32(num)] = true
			}
		}
		intersection := make(map[uint32]bool)
		for num := range winningSet {
			if handSet[num] {
				intersection[num] = true
			}
		}
		for i := cardNumber + 1; i < cardNumber+uint32(len(intersection))+1; i++ {
			copies := instances[i] + 1 + instances[cardNumber]
			instances[i] = copies
		}
		result++
	}
	for _, count := range instances {
		result += count
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
