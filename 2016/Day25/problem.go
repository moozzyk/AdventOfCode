package main

import "fmt"

func main() {
	// the input program prints reversed a + 2538 in the binary form so we need to find
	// the first number in form of %101010101010... greater than 2538 and subtract 2538
	// 2538 seems to be real puzzle input inferred from:
	// cpy 9 c
	// cpy 282 b
	// where 9 is multiplicand and 282 is multiplier -> 9 * 282 = 2538
	answer := 2
	for answer < 2538 {
		answer = (answer<<2 | 2)
	}
	fmt.Println(answer - 2538)
}
