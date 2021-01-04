package main

import "fmt"

func main() {
	fmt.Println("Problem 1:", problem1(3018458))
	fmt.Println("Problem 2:", problem2(3018458))
}

func problem1(count int) int {
	elves := make([]int, count)
	for i := 0; i < count; i++ {
		elves[i] = 1
	}

	for i := 0; ; i = (i + 1) % len(elves) {
		if elves[i] == count {
			return i + 1 // + 1 because the slice is zero based
		} else if elves[i] > 0 {
			for j := (i + 1) % len(elves); ; j = (j + 1) % len(elves) {
				if elves[j] != 0 {
					elves[i] += elves[j]
					elves[j] = 0
					break
				}
			}
		}
	}
}

func problem2(count int) int {
	elves := make([]int, count)
	for i := 0; i < count; i++ {
		elves[i] = 1
	}

	i := 0
	j := len(elves) / 2
	remaining := count
	for remaining > 1 {
		elves[i] += elves[j]
		elves[j] = 0
		remaining--

		// fmt.Println(elves, " ", i, " ", i+1, "<-", j+1)

		i = (i + 1) % len(elves)
		for elves[i] == 0 {
			i = (i + 1) % len(elves)
		}

		for elves[j] == 0 {
			j = (j + 1) % len(elves)
		}

		if (remaining & 1) == 0 {
			j = (j + 1) % len(elves)
			for elves[j] == 0 {
				j = (j + 1) % len(elves)
			}
		}

	}
	return i + 1
}

type elf struct {
	pos      int
	presents int
}

func problem2Slow(count int) int {
	elves := make([]elf, count)
	for i := 0; i < count; i++ {
		elves[i] = elf{i + 1, 1}
	}

	i := 0
	for len(elves) > 1 {

		if len(elves)%10000 == 0 {
			fmt.Println(len(elves))
		}

		j := (i + len(elves)/2) % len(elves)
		elves[i].presents += elves[j].presents
		fmt.Println(elves, " ", i, " ", elves[i].pos, "<-", elves[j].pos)
		elves = append(elves[:j], elves[j+1:]...)

		if j > i {
			i++
		}
		i = i % len(elves)
	}

	return elves[0].pos
}
