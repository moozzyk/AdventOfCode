package main

import "fmt"

func main() {
	input := ".^^^.^.^^^.^.......^^.^^^^.^^^^..^^^^^.^.^^^..^^.^.^^..^.^..^^...^.^^.^^^...^^.^.^^^..^^^^.....^...."
	fmt.Println("Problem 1:", problem(input, 40))
	fmt.Println("Problem 2:", problem(input, 400000))
}

func problem(line string, numRows int) int {
	safeCount := 0
	for i := 0; i < len(line); i++ {
		if line[i] == '.' {
			safeCount++
		}
	}

	var newLine string
	for i := 0; i < numRows-1; i++ {
		newLine = ""

		for j := 0; j < len(line); j++ {
			if (j > 0 && j < len(line)-1) && (line[j-1:j+2] == "^^." || line[j-1:j+2] == ".^^" || line[j-1:j+2] == "^.." || line[j-1:j+2] == "..^") {
				newLine += "^"
			} else if j == 0 && (line[:2] == ".^" || line[:2] == "^^") {
				newLine += "^"
			} else if j == len(line)-1 && (line[j-1:] == "^." || line[j-1:] == "^^") {
				newLine += "^"
			} else {
				newLine += "."
				safeCount++
			}
		}
		line = newLine
	}
	return safeCount
}
