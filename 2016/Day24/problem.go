package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

type node struct {
	name                   string
	size, used, avail, use int
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	file, err := os.Open("input")
	check(err)
	defer file.Close()

	maze := [][]byte{}
	scanner := bufio.NewScanner(file)
	for i := 0; scanner.Scan(); i++ {
		maze = append(maze, []byte(scanner.Text()))
	}

	source := []byte{'1', '2', '3', '4', '5', '6', '7'}

	getPermutations(source, []byte{})

	fmt.Println("Problem1: ", problem1(maze))
	fmt.Println("Problem1: ", problem2(maze))

}

var permutations = [][]byte{}

func problem1(maze [][]byte) int {

	minSteps := math.MaxInt32

	for i := 0; i < len(permutations); i++ {
		permutations[i] = append([]byte{'0'}, permutations[i]...)
		steps := findTotalSteps(maze, permutations[i])
		if steps < minSteps {
			minSteps = steps
		}
	}

	return minSteps
}

func problem2(maze [][]byte) int {

	minSteps := math.MaxInt32

	for i := 0; i < len(permutations); i++ {
		permutations[i] = append(permutations[i], '0')
		steps := findTotalSteps(maze, permutations[i])
		if steps < minSteps {
			minSteps = steps
		}
	}

	return minSteps
}

var mem = map[int]int{}

func findTotalSteps(maze [][]byte, route []byte) int {
	steps := 0
	for i := 0; i < len(route)-1; i++ {
		key := (int(route[i]) << 8) | int(route[i+1])
		if s, ok := mem[(int(route[i])<<8)|int(route[i+1])]; ok {
			steps += s
		} else if s, ok := mem[(int(route[i+1])<<8)|int(route[i])]; ok {
			steps += s
		} else {
			s := findSteps(maze, route[i], route[i+1])
			mem[key] = s
			steps += s
		}
	}
	return steps
}

type location struct {
	row, col int
}
type step struct {
	loc  location
	path []location
}

func findSteps(maze [][]byte, from, to byte) int {
	fromRow, fromCol := findLocation(maze, from)
	toRow, toCol := findLocation(maze, to)
	// fmt.Println("Finding steps from ", string(from), " (", fromRow, ", ", fromCol, ") ", " to ", string(to), " (", toRow, ", ", toCol, ")")

	s := step{loc: location{fromRow, fromCol}, path: []location{}}
	q := []step{s}
	v := []location{}

	for len(q) > 0 {

		s = q[0]
		q = q[1:]
		row := s.loc.row
		col := s.loc.col

		if visited(v, row, col) {
			continue
		}

		v = append(v, location{row, col})

		if row == toRow && col == toCol {
			return len(s.path)
		}

		if row > 0 && maze[row-1][col] != '#' {
			newPath := make([]location, len(s.path))
			copy(newPath, s.path)
			newPath = append(newPath, location{row, col})
			s1 := step{loc: location{row - 1, col}, path: newPath}
			q = append(q, s1)
		}

		if row < len(maze)-1 && maze[row+1][col] != '#' {
			newPath := make([]location, len(s.path))
			copy(newPath, s.path)
			newPath = append(newPath, location{row, col})
			s1 := step{loc: location{row + 1, col}, path: newPath}
			q = append(q, s1)
		}

		if col > 0 && maze[row][col-1] != '#' {
			newPath := make([]location, len(s.path))
			copy(newPath, s.path)
			newPath = append(newPath, location{row, col})
			s1 := step{loc: location{row, col - 1}, path: newPath}
			q = append(q, s1)
		}

		if col < len(maze[row])-1 && maze[row][col+1] != '#' {
			newPath := make([]location, len(s.path))
			copy(newPath, s.path)
			newPath = append(newPath, location{row, col})
			s1 := step{loc: location{row, col + 1}, path: newPath}
			q = append(q, s1)
		}
	}

	panic("Did not reach " + string(to))
}

func visited(path []location, row int, col int) bool {
	for i := 0; i < len(path); i++ {
		if path[i].row == row && path[i].col == col {
			return true
		}
	}

	return false
}

func findLocation(maze [][]byte, location byte) (int, int) {
	for i := 0; i < len(maze); i++ {
		for j := 0; j < len(maze[i]); j++ {
			if maze[i][j] == location {
				return i, j
			}
		}
	}

	panic("Could not find " + string(location))
}

func getPermutations(source []byte, permutation []byte) {
	if len(source) == 0 {
		newPermutation := make([]byte, len(permutation))
		copy(newPermutation, permutation)
		permutations = append(permutations, newPermutation)
		return
	}

	for i := 0; i < len(source); i++ {
		permutation = append(permutation, source[i])
		newSource := make([]byte, len(source))
		copy(newSource, source)
		newSource = append(newSource[:i], newSource[i+1:]...)
		getPermutations(newSource, permutation)
		permutation = permutation[:len(permutation)-1]
	}
}
