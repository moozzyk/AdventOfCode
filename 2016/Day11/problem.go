package main

import (
	"fmt"
	"math"
)

func main() {
	example()
	problem1()
	problem2()
}

func example() {
	floors := make([][]bool, 4)

	// F4 .  .  .  .  .
	// F3 .  .  .  LG .
	// F2 .  HG .  .  .
	// F1 E  .  HM .  LM
	floors[3] = []bool{false, false, false, false}
	floors[2] = []bool{false, false, true, false}
	floors[1] = []bool{true, false, false, false}
	floors[0] = []bool{false, true, false, true}

	s := state{floors, 0, 0}
	names := []string{" HG ", " HM ", " LG ", " LM "}
	printState(s, names)
	minSteps := solve(state{floors, 0, 0})
	fmt.Println("Solved in", minSteps, "steps")
	fmt.Println()
}

func problem1() {
	floors := make([][]bool, 4)
	// F4
	// F3                                 Pr-G Pr-M Ru-G Ru-M
	// F2                  Pl-M      St-M
	// F1 E Th-G Th-M Pl-G      St-G
	floors[3] = []bool{false, false, false, false, false, false, false, false, false, false}
	floors[2] = []bool{false, false, false, false, false, false, true, true, true, true}
	floors[1] = []bool{false, false, false, true, false, true, false, false, false, false}
	floors[0] = []bool{true, true, true, false, true, false, false, false, false, false}

	s := state{floors, 0, 0}
	names := []string{"Th-G", "Th-M", "Pl-G", "Pl-M", "St-G", "St-M", "Pr-G", "Pr-M", "Ru-G", "Ru-M"}
	printState(s, names)
	minSteps := solve(state{floors, 0, 0})
	fmt.Println("Solved in", minSteps, "steps")
	fmt.Println()
}

func problem2() {
	floors := make([][]bool, 4)
	// F4
	// F3                                 Pr-G Pr-M Ru-G Ru-M
	// F2                  Pl-M      St-M
	// F1 E Th-G Th-M Pl-G      St-G                          El-G El-M Di-G Di-M
	floors[3] = []bool{false, false, false, false, false, false, false, false, false, false, false, false, false, false}
	floors[2] = []bool{false, false, false, false, false, false, true, true, true, true, false, false, false, false}
	floors[1] = []bool{false, false, false, true, false, true, false, false, false, false, false, false, false, false}
	floors[0] = []bool{true, true, true, false, true, false, false, false, false, false, true, true, true, true}

	s := state{floors, 0, 0}
	var names = []string{"Th-G", "Th-M", "Pl-G", "Pl-M", "St-G", "St-M", "Pr-G", "Pr-M", "Ru-G", "Ru-M", "El-G", "El-M", "Di-G", "Di-M"}
	printState(s, names)
	minSteps := solve(state{floors, 0, 0})
	fmt.Println("Solved in", minSteps, "steps")
	fmt.Println()
}

type state struct {
	floors [][]bool
	floor  int
	steps  int
}

func (s state) isSolved() bool {
	lastFloor := len(s.floors) - 1
	for i := 0; i < len(s.floors[lastFloor]); i++ {
		if !s.floors[lastFloor][i] {
			return false
		}
	}

	return true
}

func (s state) isValid() bool {
	for i := 0; i < len(s.floors); i++ {
		for j := 1; j < len(s.floors[i]); j += 2 {
			if s.floors[i][j] && !s.floors[i][j-1] {
				for k := 0; k < len(s.floors[i]); k += 2 {
					if s.floors[i][k] {
						return false
					}
				}
			}
		}
	}

	return true
}

func (s state) computeHash() int64 {
	hashValue := int64(s.floor)
	var bit int64 = 4
	for i := 0; i < len(s.floors); i++ {
		for j := 0; j < len(s.floors[i]); j++ {
			if s.floors[i][j] {
				hashValue = hashValue | bit
			}
			bit = bit << 1
		}
	}
	return hashValue
}

func swap(floor1 []bool, floor2 []bool, i int) {
	if floor2[i] {
		panic("Target not empty")
	}
	tmp := floor1[i]
	floor1[i] = floor2[i]
	floor2[i] = tmp
}

func cloneFloors(floors [][]bool) [][]bool {
	c := make([][]bool, len(floors))
	for i := 0; i < len(floors); i++ {
		c[i] = make([]bool, len(floors[i]))
		copy(c[i], floors[i])
	}
	return c
}

func solve(s state) int {
	var minSteps = math.MaxInt32
	var cycles = map[int64]bool{}

	q := []state{s}

	for len(q) > 0 {
		h := q[0]
		q = q[1:]

		if h.floor == len(h.floors) {
			continue
		}

		if h.isSolved() {
			if h.steps < minSteps {
				minSteps = h.steps
			}
			continue
		}

		if !h.isValid() {
			continue
		}

		floorHash := h.computeHash()
		if _, ok := cycles[floorHash]; !ok {
			cycles[floorHash] = true
		} else {
			continue
		}

		q = append(q, tryMove1(h, true)...)
		q = append(q, tryMove2(h, true)...)
		q = append(q, tryMove1(h, false)...)
		q = append(q, tryMove2(h, false)...)
	}

	return minSteps
}

func hasPartsBelow(s state) bool {
	for i := s.floor - 1; i >= 0; i-- {
		for j := 0; j < len(s.floors[i]); j++ {
			if s.floors[i][j] {
				return true
			}
		}
	}

	return false
}

func canMove(s state, up bool) bool {
	if up && s.floor == len(s.floors)-1 {
		return false
	}

	if !up && (s.floor == 0 || !hasPartsBelow(s)) {
		return false
	}

	return true
}

func tryMove1(s state, up bool) []state {
	items := []state{}

	if !canMove(s, up) {
		return items
	}

	newFloor := s.floor
	if up {
		newFloor++
	} else {
		newFloor--
	}

	for i := 0; i < len(s.floors[s.floor]); i++ {
		if s.floors[s.floor][i] {
			newFloors := cloneFloors(s.floors)
			swap(newFloors[s.floor], newFloors[newFloor], i)
			items = append(items, state{newFloors, newFloor, s.steps + 1})
		}
	}

	return items
}

func tryMove2(s state, up bool) []state {
	items := []state{}

	if !canMove(s, up) {
		return items
	}

	newFloor := s.floor
	if up {
		newFloor++
	} else {
		newFloor--
	}

	for i := 0; i < len(s.floors[s.floor]); i++ {
		for j := i + 1; j < len(s.floors[s.floor]); j++ {
			if s.floors[s.floor][i] && s.floors[s.floor][j] {
				newFloors := cloneFloors(s.floors)
				swap(newFloors[s.floor], newFloors[newFloor], i)
				swap(newFloors[s.floor], newFloors[newFloor], j)
				items = append(items, state{newFloors, newFloor, s.steps + 1})
			}
		}
	}

	return items
}

func printState(s state, names []string) {
	for i := len(s.floors) - 1; i >= 0; i-- {
		if i == s.floor {
			fmt.Print(" *|")
		} else {
			fmt.Print("  |")
		}

		for j := 0; j < len(s.floors[i]); j++ {
			if s.floors[i][j] {
				fmt.Print(" " + names[j] + " ")
			} else {
				fmt.Print("      ")
			}
		}
		fmt.Println()
	}
}
