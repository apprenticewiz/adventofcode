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

func buildGears(contents string) map[Position]rune {
	gearLocs := make(map[Position]rune)
	lines := strings.Split(contents, "\n")
	for row, line := range lines {
		for col, ch := range line {
			if ch == '*' {
				gearLocs[Position{Row: row, Col: col}] = ch
			}
		}
	}
	return gearLocs
}

func checkGears(numberLocs map[Position]string, gearLocs map[Position]rune) uint32 {
	result := uint32(0)
	for gearLoc, _ := range gearLocs {
		adjacentCount := 0
		prod := 1
		for numberLoc, numberStr := range numberLocs {
			found := false
			numberRow := numberLoc.Row
			numberColStart := numberLoc.Col
			numberColLast := numberLoc.Col + len(numberStr) - 1
			neighbors := [...]Position{
				{Row: -1, Col: -1}, {Row: -1, Col: 0}, {Row: -1, Col: 1},
				{Row: 0, Col: -1}, {Row: 0, Col: 1},
				{Row: 1, Col: -1}, {Row: 1, Col: 0}, {Row: 1, Col: 1},
			}
			for _, neighbor := range neighbors {
				pos := Position{Row: gearLoc.Row + neighbor.Row, Col: gearLoc.Col + neighbor.Col}
				if pos.Row == numberRow && pos.Col >= numberColStart && pos.Col <= numberColLast {
					found = true
					break
				}
			}
			if found {
				adjacentCount++
				number, _ := strconv.Atoi(numberStr)
				prod *= number
			}
		}
		if adjacentCount == 2 {
			result += uint32(prod)
		}
	}
	return result
}

func process(contents string) uint32 {
	numberLocs := buildNumbers(contents)
	gearLocs := buildGears(contents)
	return checkGears(numberLocs, gearLocs)
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
