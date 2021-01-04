package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"strconv"
	"strings"
)

func main() {
	dat, err := ioutil.ReadFile("input")
	check(err)
	directions := strings.Split(string(dat), ", ")
	problem1(directions)
	problem2(directions)
}

func problem1(directions []string) {
	var x, y, dir int = 0, 0, 0

	for i := 0; i < len(directions); i++ {
		direction := directions[i][0]
		steps, _ := strconv.Atoi(directions[i][1:len(directions[i])])
		if direction == 'R' {
			dir = (dir + 1) & 3
		} else {
			dir = (dir - 1) & 3
		}

		switch dir {
		case 0:
			y += steps
		case 1:
			x += steps
		case 2:
			y -= steps
		case 3:
			x -= steps
		}
	}

	fmt.Println(math.Abs(float64(x)) + math.Abs(float64(y)))
}

type point struct {
	x int
	y int
}

type locations map[point]bool

func problem2(directions []string) {

	visited := make(locations)

	var dir int
	var location = point{x: 0, y: 0}
	visited[location] = true

	for i := 0; i < len(directions); i++ {
		direction := directions[i][0]
		steps, _ := strconv.Atoi(directions[i][1:len(directions[i])])
		if direction == 'R' {
			dir = (dir + 1) & 3
		} else {
			dir = (dir - 1) & 3
		}

		for j := 0; j < steps; j++ {
			switch dir {
			case 0:
				location.y++
			case 1:
				location.x++
			case 2:
				location.y--
			case 3:
				location.x--
			}

			// fmt.Println("%v", location)
			if _, ok := visited[location]; ok {
				fmt.Println(math.Abs(float64(location.x)) + math.Abs(float64(location.y)))
				return
			}

			visited[location] = true
		}
	}
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}
