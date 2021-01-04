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

	screen := problem(lines)

	count := 0
	for i := 0; i < len(screen); i++ {
		for j := 0; j < len(screen[i]); j++ {
			count += int(screen[i][j])
		}
		fmt.Println(screen[i])
	}

	fmt.Println(count)

}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func problem(lines []string) [][]byte {
	screen := make([][]byte, 6)
	for i := 0; i < len(screen); i++ {
		screen[i] = make([]byte, 50)
	}

	for i := 0; i < len(lines); i++ {
		if lines[i][:4] == "rect" {
			rect(screen, lines[i])
		} else if lines[i][:10] == "rotate row" {
			rotateRow(screen, lines[i])
		} else {
			rotateCol(screen, lines[i])
		}
	}
	return screen
}

var rectExp = regexp.MustCompile(`\d+`)

func rect(screen [][]byte, line string) {
	match := rectExp.FindAllString(line, -1)
	cols, _ := strconv.Atoi(match[0])
	rows, _ := strconv.Atoi(match[1])

	for c := 0; c < cols; c++ {
		for r := 0; r < rows; r++ {
			screen[r][c] = 1
		}
	}
}

func rotateCol(screen [][]byte, line string) {
	match := rectExp.FindAllString(line, -1)
	col, _ := strconv.Atoi(match[0])
	times, _ := strconv.Atoi(match[1])

	for i := 0; i < times; i++ {
		rotateColBy1(screen, col)
	}
}

// Slooow
func rotateColBy1(screen [][]byte, col int) {
	last := screen[len(screen)-1][col]

	for i := len(screen) - 2; i >= 0; i-- {
		screen[i+1][col] = screen[i][col]
	}

	screen[0][col] = last
}

func rotateRow(screen [][]byte, line string) {
	match := rectExp.FindAllString(line, -1)
	row, _ := strconv.Atoi(match[0])
	times, _ := strconv.Atoi(match[1])

	for i := 0; i < times; i++ {
		rotateRowBy1(screen, row)
	}
}

// Slooow
func rotateRowBy1(screen [][]byte, row int) {
	last := screen[row][len(screen[0])-1]

	for i := len(screen[row]) - 2; i >= 0; i-- {
		screen[row][i+1] = screen[row][i]
	}

	screen[row][0] = last
}
