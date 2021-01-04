package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

func main() {
	file, err := os.Open("input")
	check(err)
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for i := 0; scanner.Scan(); i++ {
		lines = append(lines, scanner.Text())
	}

	problem1(lines)
	problem2(lines)
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func problem1(lines []string) {
	out := []byte{}

	for i := 0; i < len(lines); i++ {
		line := lines[i]
		for j := 0; j < len(line); {
			if line[j] == '(' {
				numChars, repetitions, markerLength := parseMarker(line, j)
				j += markerLength
				seq := []byte(line[j : j+numChars])
				for k := 0; k < repetitions; k++ {
					out = append(out, seq...)
				}
				j += numChars
			} else {
				out = append(out, line[j])
				j++
			}
		}
	}

	fmt.Println(len(out))
}

func problem2(lines []string) {
	total := 0
	for i := 0; i < len(lines); i++ {
		total += countChars(lines[i], 0)
	}

	fmt.Println(total)
}

func countChars(line string, j int) int {
	count := 0
	for j < len(line) {
		if line[j] == '(' {
			numChars, repetitions, markerLength := parseMarker(line, j)
			j += markerLength
			seq := []byte(line[j : j+numChars])
			count += repetitions * countChars(string(seq), 0)
			j += numChars
		} else {
			j++
			count++
		}
	}

	return count
}

var markerExp = regexp.MustCompile(`\d+`)

func parseMarker(line string, markerBegin int) (int, int, int) {
	markerEnd := markerBegin
	for line[markerEnd] != ')' {
		markerEnd++
	}
	markerEnd++

	match := markerExp.FindAllString(line[markerBegin:markerEnd], -1)

	numChars, _ := strconv.Atoi(match[0])
	repetitions, _ := strconv.Atoi(match[1])

	return numChars, repetitions, markerEnd - markerBegin
}
