package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
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
	count := 0
	for i := 0; i < len(lines); i++ {
		s := strings.TrimSpace(lines[i])

		var a, b, c int
		fmt.Sscanf(s, "%d%d%d", &a, &b, &c)
		count += isValidTriangle(a, b, c)
	}

	fmt.Println(count)
}

func problem2(lines []string) {
	count := 0
	for i := 0; i < len(lines); i += 3 {
		//brute force rules!
		var a1, b1, c1, a2, b2, c2, a3, b3, c3 int
		fmt.Sscanf(lines[i], "%d%d%d", &a1, &a2, &a3)
		fmt.Sscanf(lines[i+1], "%d%d%d", &b1, &b2, &b3)
		fmt.Sscanf(lines[i+2], "%d%d%d", &c1, &c2, &c3)

		count += isValidTriangle(a1, b1, c1) + isValidTriangle(a2, b2, c2) + isValidTriangle(a3, b3, c3)
	}
	fmt.Println(count)
}

func isValidTriangle(a, b, c int) int {
	if a+b > c && a+c > b && b+c > a {
		return 1
	}
	return 0
}
func check(e error) {
	if e != nil {
		panic(e)
	}
}
