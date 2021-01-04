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

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func problem1(lines []string) {
	count := 0
	for i := 0; i < len(lines); i++ {
		if isABBA(lines[i]) {
			count++
		}
	}

	fmt.Println(count)
}

func isABBA(line string) bool {
	mode := 0
	found := false
	for i := 0; i < len(line); i++ {
		if line[i] == '[' {
			mode = 1
		} else if line[i] == ']' {
			mode = 0
		} else {
			if i < len(line)-3 {
				if line[i] != line[i+1] && line[i] == line[i+3] && line[i+1] == line[i+2] {
					if mode == 1 {
						return false
					}
					found = true
				}
			}
		}
	}

	return found
}

func problem2(lines []string) {
	count := 0
	for i := 0; i < len(lines); i++ {
		if isABA(lines[i]) {
			count++
		}
	}

	fmt.Println(count)
}

func isABA(line string) bool {
	mode := 0
	out := map[string]bool{}
	var in []string

	for i := 0; i < len(line); i++ {
		if line[i] == '[' {
			mode = 1
		} else if line[i] == ']' {
			mode = 0
		} else {
			if i < len(line)-2 {
				if line[i] == line[i+2] && line[i] != line[i+1] && line[i+1] != '[' && line[i+1] != ']' {
					if mode == 0 {
						out[string(line[i+1])+line[i:i+2]] = true
					} else {
						in = append(in, line[i:i+3])
					}
				}
			}
		}
	}

	for i := 0; i < len(in); i++ {
		if _, ok := out[in[i]]; ok {
			return true
		}
	}
	return false
}
