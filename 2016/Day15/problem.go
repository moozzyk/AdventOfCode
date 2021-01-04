package main

import (
	"fmt"
	"math"
)

type disc struct {
	size, initialPosition int
}

// Disc #1 has 13 positions; at time=0, it is at position 11.
// Disc #2 has 5 positions; at time=0, it is at position 0.
// Disc #3 has 17 positions; at time=0, it is at position 11.
// Disc #4 has 3 positions; at time=0, it is at position 0.
// Disc #5 has 7 positions; at time=0, it is at position 2.
// Disc #6 has 19 positions; at time=0, it is at position 17

func main() {
	fmt.Println(problem([]disc{{13, 11}, {5, 0}, {17, 11}, {3, 0}, {7, 2}, {19, 17}}))
	fmt.Println(problem([]disc{{13, 11}, {5, 0}, {17, 11}, {3, 0}, {7, 2}, {19, 17}, {11, 0}}))
}

func problem(discs []disc) int {
	for i := discs[0].size - discs[0].initialPosition - 1; i < math.MaxInt32; i += discs[0].size {
		for j := 0; j < len(discs); j++ {
			discPos := (1 + i + j + discs[j].initialPosition) % discs[j].size
			if discPos != 0 {
				break
			} else if j == len(discs)-1 {
				return i
			}
		}
	}

	return -1
}
