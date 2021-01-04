package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input")
	check(err)
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for i := 0; scanner.Scan(); i++ {
		lines = append(lines, scanner.Text())
	}

	problem1(lines)
	problem2(lines)
}

type freq struct {
	letter      byte
	occurrences int
}

type sortFreq []freq

func (f sortFreq) Len() int { return len(f) }

func (f sortFreq) Less(i, j int) bool {
	return f[i].occurrences < f[j].occurrences ||
		(f[i].occurrences == f[j].occurrences && f[i].letter > f[j].letter)
}

func (f sortFreq) Swap(i, j int) { f[i], f[j] = f[j], f[i] }

func problem1(lines []string) {
	sum := 0
	for i := 0; i < len(lines); i++ {
		pivot := strings.LastIndex(lines[i], "-")
		left := lines[i][0:pivot]

		freqs := make([]freq, 26)
		for j := 0; j < len(left); j++ {
			if left[j] != '-' {
				idx := left[j] - 'a'
				freqs[idx].letter = left[j]
				freqs[idx].occurrences++
			}
		}

		right := lines[i][pivot+1:]
		k := 0
		for ; right[k] >= '0' && right[k] <= '9'; k++ {
		}
		sectorID, _ := strconv.Atoi(right[0:k])
		checkSum := right[k+1 : len(right)-1]

		sort.Sort(sort.Reverse(sortFreq(freqs)))
		if validate(freqs, checkSum) {
			sum += sectorID
		}
	}
	fmt.Println(sum)
}

func validate(freqs []freq, checksum string) bool {
	for i := 0; i < len(checksum); i++ {
		if checksum[i] != freqs[i].letter {
			return false
		}
	}
	return true
}

func problem2(lines []string) {
	for i := 0; i < len(lines); i++ {
		pivot := strings.LastIndex(lines[i], "-")
		sectorID, _ := strconv.Atoi(lines[i][pivot+1 : len(lines[i])-7])
		shift := byte(sectorID % 26)

		var result []byte
		for j := 0; j < pivot; j++ {
			if lines[i][j] == '-' {
				result = append(result, ' ')
			} else {
				result = append(result, ((lines[i][j]-'a'+shift)%26)+'a')
			}
		}

		if strings.Index(string(result), "northpole") != -1 {
			fmt.Println(lines[i], string(result), sectorID)
		}
	}
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}
