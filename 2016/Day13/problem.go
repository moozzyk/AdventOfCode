package main

import "fmt"

func main() {
	problem1()
	problem2()
}

func problem1() {
	printMaze(buildMaze(1364, 50, 50))
}

func problem2() {
	findPaths(buildMaze(1364, 30, 30), 1, 1, 0)
	fmt.Println("Problem 2: Visited ", len(reached))
}

func buildMaze(seed, rows, cols int) [][]byte {
	maze := make([][]byte, rows)

	for y := 0; y < len(maze); y++ {
		maze[y] = make([]byte, cols)
		for x := 0; x < len(maze[y]); x++ {
			value := x*x + 3*x + 2*x*y + y + y*y + seed
			maze[y][x] = byte(countBits(value) & 1)
		}
	}

	return maze
}

func countBits(n int) int {
	count := 0
	for n > 0 {
		n = n & (n - 1)
		count++
	}

	return count
}

type location struct {
	x, y int
}

var reached = map[location]bool{}

func findPaths(maze [][]byte, x, y int, steps int) {
	if y < 0 || x < 0 || maze[y][x] != 0 {
		return
	}

	reached[location{x, y}] = true

	if steps == 50 {
		return
	}
	if (x == 0 || maze[y][x-1] != 0) && (y == 0 || maze[y-1][x] != 0) && maze[y+1][x] != 0 && maze[y][x+1] != 0 {
		return
	}

	maze[y][x] = 2
	findPaths(maze, x-1, y, steps+1)
	findPaths(maze, x+1, y, steps+1)
	findPaths(maze, x, y-1, steps+1)
	findPaths(maze, x, y+1, steps+1)
	maze[y][x] = 0
}

func printMaze(maze [][]byte) {
	for y := 0; y < len(maze); y++ {
		for x := 0; x < len(maze[y]); x++ {
			if x == 31 && y == 39 {
				if maze[y][x] != 0 {
					panic("Invalid maze")
				}
				fmt.Print("o")
			} else if maze[y][x] == 0 {
				fmt.Print(" ")
			} else if maze[y][x] == 1 {
				fmt.Print("#")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}
