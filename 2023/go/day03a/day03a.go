package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
	"unicode"
)

type Position struct {
	Row int
	Col int
}

func usage(progname string) {
	log.Fatalf("usage: %s <file>\n", progname)
	os.Exit(1)
}

func buildNumbers(contents string) map[Position]string {
	numberLocs := make(map[Position]string)
	lines := strings.Split(contents, "\n")
	scanningNumber := false
	currentPos := Position{Row: -1, Col: -1}
	number := []rune{}
	for row, line := range lines {
		for col, ch := range line {
			if scanningNumber {
				if unicode.IsDigit(ch) {
					number = append(number, ch)
				} else {
					numberLocs[currentPos] = string(number)
					scanningNumber = false
					number = []rune{}
				}
			} else {
				if unicode.IsDigit(ch) {
					number = append(number, ch)
					currentPos = Position{Row: row, Col: col}
					scanningNumber = true
				}
			}
		}
		if scanningNumber {
			numberLocs[currentPos] = string(number)
			scanningNumber = false
			number = []rune{}
		}
	}
	return numberLocs
}

func buildParts(contents string) map[Position]rune {
	partLocs := make(map[Position]rune)
	lines := strings.Split(contents, "\n")
	for row, line := range lines {
		for col, ch := range line {
			if !unicode.IsDigit(ch) && ch != '.' {
				partLocs[Position{Row: row, Col: col}] = ch
			}
		}
	}
	return partLocs
}

func checkParts(numberLocs map[Position]string, partLocs map[Position]rune) uint32 {
	result := uint32(0)
	for numberLoc, numberStr := range numberLocs {
		adjacentCount := 0
		numberRow := numberLoc.Row
		numberColStart := numberLoc.Col
		numberColLast := numberLoc.Col + len(numberStr)
		for numberCol := numberColStart; numberCol < numberColLast; numberCol++ {
			neighbors := [...]Position{
				{Row: -1, Col: -1}, {Row: -1, Col: 0}, {Row: -1, Col: 1},
				{Row: 0, Col: -1}, {Row: 0, Col: 1},
				{Row: 1, Col: -1}, {Row: 1, Col: 0}, {Row: 1, Col: 1},
			}
			for partLoc, _ := range partLocs {
				for _, neighbor := range neighbors {
					pos := Position{Row: numberRow + neighbor.Row, Col: numberCol + neighbor.Col}
					if partLoc.Row == pos.Row && partLoc.Col == pos.Col {
						adjacentCount++
						break
					}
				}
			}
		}
		if adjacentCount != 0 {
			number, _ := strconv.Atoi(numberStr)
			result += uint32(number)
		}
	}
	return result
}

func process(contents string) uint32 {
	numberLocs := buildNumbers(contents)
	partLocs := buildParts(contents)
	return checkParts(numberLocs, partLocs)
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
