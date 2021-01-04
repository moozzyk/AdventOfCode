package main

import (
	"bufio"
	"fmt"
	"os"
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

func problem1(lines []string) {
	var msg string
	for i := 0; i < len(lines[0]); i++ {
		freq := make([]int, 26)
		max := 0
		var letter byte

		for j := 0; j < len(lines); j++ {
			idx := lines[j][i] - 'a'
			freq[idx]++
			if freq[idx] > max {
				max = freq[idx]
				letter = lines[j][i]
			}
		}
		msg += string(letter)
	}
	fmt.Println(msg)
}

func problem2(lines []string) {
	var msg string
	for i := 0; i < len(lines[0]); i++ {
		freq := make([]int, 26)

		for j := 0; j < len(lines); j++ {
			idx := lines[j][i] - 'a'
			freq[idx]++
		}

		var letter byte
		minFreq := len(lines) + 1
		var k byte
		for ; k < 26; k++ {
			if freq[k] > 0 && freq[k] < minFreq {
				minFreq = freq[k]
				letter = k + 'a'
			}
		}

		msg += string(letter)
	}
	fmt.Println(msg)
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}
