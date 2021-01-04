package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
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

	lines := []string{}
	scanner := bufio.NewScanner(file)
	for i := 0; scanner.Scan(); i++ {
		lines = append(lines, scanner.Text())
	}

	nodes := []node{}
	for i := 2; i < len(lines); i++ {
		s := strings.Fields(lines[i])
		name := s[0]
		size, _ := strconv.Atoi(s[1][:len(s[1])-1])
		used, _ := strconv.Atoi(s[2][:len(s[2])-1])
		avail, _ := strconv.Atoi(s[3][:len(s[3])-1])
		use, _ := strconv.Atoi(s[3][:len(s[3])-1])

		nodes = append(nodes, node{name, size, used, avail, use})
	}

	fmt.Println("Node count:", problem1(nodes))
	problem2(nodes)
}

func problem1(nodes []node) int {
	count := 0
	for i := 0; i < len(nodes); i++ {
		if nodes[i].used == 0 {
			continue
		}
		for j := 0; j < len(nodes); j++ {
			if i == j {
				continue
			}

			if nodes[i].used <= nodes[j].avail {
				count++
			}
		}
	}

	return count
}

func problem2(nodes []node) {
	Size := 31
	grid := make([][]node, Size)

	zeroNodeCapacity := 0
	for y := 0; y < len(grid); y++ {
		grid[y] = make([]node, Size)
		for x := 0; x < len(grid[y]); x++ {
			grid[y][x] = nodes[x*Size+y]
			if grid[y][x].used == 0 {
				zeroNodeCapacity = grid[y][x].avail
			}
		}
	}

	fmt.Println("Grid")

	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[y]); x++ {

			c := "."

			if x == 0 && y == 0 {
				c = "*"
			} else if grid[y][x].used == 0 {
				c = "_"
			} else if grid[y][x].used > zeroNodeCapacity {
				c = "#"
			} else if y == 0 && x == len(grid[y])-1 {
				c = "G"
			}

			fmt.Print(c, " ")
		}
		fmt.Println()
	}
}
