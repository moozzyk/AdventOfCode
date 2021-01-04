package main

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"math"
)

func main() {
	problem1("njfxhljp")
	problem2("njfxhljp")
}

type state struct {
	hash string
	x, y int
}

func problem1(input string) {
	cycle := map[int]bool{}

	que := []state{{input, 0, 0}}
	for len(que) > 0 {
		current := que[0]
		que = que[1:]

		if current.x == 3 && current.y == 3 {
			fmt.Println("Shortest path: ", current.hash[len(input):])
			return
		}

		tmp := md5.Sum([]byte(current.hash))
		hash := hex.EncodeToString(tmp[:])

		key := 0
		for i := 0; i < 4; i++ {
			if hash[i] > 'a' && hash[i] <= 'f' {
				key |= 1 << uint(i)
			}
		}

		key |= current.y << 4
		key |= current.x << 6

		if _, ok := cycle[key]; ok {
			continue
		}

		cycle[key] = true

		if hash[0] > 'a' && hash[0] <= 'f' && current.y > 0 {
			que = append(que, state{current.hash + "U", current.x, current.y - 1})
		}

		if hash[1] > 'a' && hash[1] <= 'f' && current.y < 3 {
			que = append(que, state{current.hash + "D", current.x, current.y + 1})
		}

		if hash[2] > 'a' && hash[2] <= 'f' && current.x > 0 {
			que = append(que, state{current.hash + "L", current.x - 1, current.y})
		}

		if hash[3] > 'a' && hash[3] <= 'f' && current.x < 3 {
			que = append(que, state{current.hash + "R", current.x + 1, current.y})
		}
	}
}

func problem2(input string) {
	maxSteps := math.MinInt32

	que := []state{{input, 0, 0}}
	for len(que) > 0 {
		current := que[0]
		que = que[1:]

		if current.x == 3 && current.y == 3 {
			pathLength := len(current.hash) - len(input)

			if pathLength > maxSteps {
				maxSteps = pathLength
			}

			continue
		}

		tmp := md5.Sum([]byte(current.hash))
		hash := hex.EncodeToString(tmp[:])

		if hash[0] > 'a' && hash[0] <= 'f' && current.y > 0 {
			que = append(que, state{current.hash + "U", current.x, current.y - 1})
		}

		if hash[1] > 'a' && hash[1] <= 'f' && current.y < 3 {
			que = append(que, state{current.hash + "D", current.x, current.y + 1})
		}

		if hash[2] > 'a' && hash[2] <= 'f' && current.x > 0 {
			que = append(que, state{current.hash + "L", current.x - 1, current.y})
		}

		if hash[3] > 'a' && hash[3] <= 'f' && current.x < 3 {
			que = append(que, state{current.hash + "R", current.x + 1, current.y})
		}
	}

	fmt.Println("Max steps: ", maxSteps)
}
