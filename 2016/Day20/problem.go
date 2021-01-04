package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"sort"
	"strconv"
)

func main() {
	file, err := os.Open("input")
	check(err)
	defer file.Close()

	lines := []string{}
	scanner := bufio.NewScanner(file)
	for i := 0; scanner.Scan(); i++ {
		lines = append(lines, scanner.Text())
	}

	ranges := ipRanges{}
	for i := 0; i < len(lines); i++ {
		match := rangeExp.FindAllString(lines[i], -1)
		from, _ := strconv.Atoi(match[0])
		to, _ := strconv.Atoi(match[1])
		ranges = append(ranges, ipRange{from, to})
	}

	sort.Sort(ranges)

	fmt.Println("First free ip:", problem1(ranges))
	fmt.Println("Free ips count:", problem2(ranges))
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type ipRange struct {
	from, to int
}

type ipRanges []ipRange

func (r ipRanges) Len() int { return len(r) }

func (r ipRanges) Less(i, j int) bool {
	return r[i].from < r[j].from ||
		(r[i].from == r[j].from && r[i].to > r[j].to)
}

func (r ipRanges) Swap(i, j int) { r[i], r[j] = r[j], r[i] }

var rangeExp = regexp.MustCompile(`\d+`)

func problem1(ranges []ipRange) int {
	mergedRanges := ranges[0]
	for i := 0; i < len(ranges); i++ {
		// fmt.Println(mergedRanges, ranges[i])
		if ranges[i].from > mergedRanges.to+1 {
			break
		}
		if ranges[i].to > mergedRanges.to {
			mergedRanges.to = ranges[i].to
		}
	}

	return mergedRanges.to + 1
}

func problem2(ranges []ipRange) int {

	current := 0
	for current < len(ranges)-1 {
		if ranges[current+1].from <= ranges[current].to+1 {
			if ranges[current+1].to > ranges[current].to {
				ranges[current].to = ranges[current+1].to
			}
			ranges = append(ranges[:current+1], ranges[current+2:]...)
		} else {
			current++
		}
	}

	free := 0
	for i := 0; i < len(ranges)-1; i++ {
		free += ranges[i+1].from - ranges[i].to - 1
	}

	return free
}
