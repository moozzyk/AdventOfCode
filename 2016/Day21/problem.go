package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"strconv"
	"strings"
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

	fmt.Println("Problem 1:", string(problem1([]byte("abcdefgh"), lines)))

	for i, j := 0, len(lines)-1; i < j; i, j = i+1, j-1 {
		lines[i], lines[j] = lines[j], lines[i]
	}

	fmt.Println("Problem 2:", string(problem2([]byte("fbgdceah"), lines)))
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func problem1(password []byte, lines []string) []byte {
	for i := 0; i < len(lines); i++ {
		split := strings.Split(lines[i], " ")
		switch split[0] {
		case "move":
			x, _ := strconv.Atoi(split[2])
			y, _ := strconv.Atoi(split[5])
			password = move(password, x, y)
		case "swap":
			if split[1] == "position" {
				x, _ := strconv.Atoi(split[2])
				y, _ := strconv.Atoi(split[5])
				password = swap(password, x, y)
			} else {
				x := bytes.IndexByte(password, split[2][0])
				y := bytes.IndexByte(password, split[5][0])
				password = swap(password, x, y)
			}
		case "reverse":
			x, _ := strconv.Atoi(split[2])
			y, _ := strconv.Atoi(split[4])
			if x > y {
				password = reverse(password, y, x)
			} else {
				password = reverse(password, x, y)
			}
		case "rotate":
			if split[1] == "based" {
				password = rotateByLetter(password, split[6][0])
			} else {
				steps, _ := strconv.Atoi(split[2])
				if split[1] == "left" {
					password = rotate(password, steps)
				} else {
					password = rotate(password, len(password)-steps)
				}
			}
		}
	}

	return password
}

func problem2(password []byte, lines []string) []byte {
	for i := 0; i < len(lines); i++ {
		split := strings.Split(lines[i], " ")
		switch split[0] {
		case "move":
			x, _ := strconv.Atoi(split[2])
			y, _ := strconv.Atoi(split[5])
			password = move(password, y, x)
		case "swap":
			if split[1] == "position" {
				x, _ := strconv.Atoi(split[2])
				y, _ := strconv.Atoi(split[5])
				password = swap(password, x, y)
			} else {
				x := bytes.IndexByte(password, split[2][0])
				y := bytes.IndexByte(password, split[5][0])
				password = swap(password, x, y)
			}
		case "reverse":
			x, _ := strconv.Atoi(split[2])
			y, _ := strconv.Atoi(split[4])
			if x > y {
				password = reverse(password, y, x)
			} else {
				password = reverse(password, x, y)
			}
		case "rotate":
			if split[1] == "based" {
				tmp := password

				for k := 0; k <= len(password); k++ {
					tmp = rotate(tmp, 1)
					r := rotateByLetter(tmp, split[6][0])
					if string(r) == string(password) {
						password = tmp
						break
					}
				}
			} else {
				password = reverse(password, 0, len(password)-1)
				steps, _ := strconv.Atoi(split[2])
				if split[1] == "left" {
					password = rotate(password, steps)
				} else {
					password = rotate(password, len(password)-steps)
				}
				password = reverse(password, 0, len(password)-1)
			}
		}
	}

	return password
}

func move(password []byte, x, y int) []byte {

	letter := password[x]
	password = append(password[:x], password[x+1:]...)
	password = append(password[:y], append([]byte{letter}, password[y:]...)...)

	return password
}

func swap(password []byte, x, y int) []byte {
	password[x], password[y] = password[y], password[x]
	return password
}

func reverse(password []byte, x, y int) []byte {
	for ; x < y; x, y = x+1, y-1 {
		password = swap(password, x, y)
	}

	return password
}

func rotate(password []byte, steps int) []byte {
	return append(password[steps:], password[:steps]...)
}

func rotateByLetter(password []byte, letter byte) []byte {
	index := bytes.IndexByte(password, letter)
	if index >= 4 {
		index++
	}
	index++
	index = index % len(password)
	return rotate(password, len(password)-index)
}
