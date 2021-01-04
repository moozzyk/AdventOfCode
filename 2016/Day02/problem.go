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
	keypad := [][]string{
		{"1", "2", "3"},
		{"4", "5", "6"},
		{"7", "8", "9"},
	}

	solve(lines, keypad, 1, 1)
}

func problem2(lines []string) {
	keypad := [][]string{
		{"*", "*", "1", "*", "*"},
		{"*", "2", "3", "4", "*"},
		{"5", "6", "7", "8", "9"},
		{"*", "A", "B", "C", "*"},
		{"*", "*", "D", "*", "*"},
	}

	solve(lines, keypad, 2, 0)
}

func solve(lines []string, keypad [][]string, x, y int) {
	var number string
	for i := 0; i < len(lines); i++ {
		for j := 0; j < len(lines[i]); j++ {
			switch lines[i][j] {
			case 'U':
				if y > 0 && keypad[x][y-1] != "*" {
					y--
				}
			case 'D':
				if y < len(keypad)-1 && keypad[x][y+1] != "*" {
					y++
				}
			case 'L':
				if x > 0 && keypad[x-1][y] != "*" {
					x--
				}
			case 'R':
				if x < len(keypad[x])-1 && keypad[x+1][y] != "*" {
					x++
				}
			}
		}
		number += keypad[y][x]
	}
	fmt.Println(number)
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}
