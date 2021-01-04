package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input")
	check(err)
	defer file.Close()

	program := [][]string{}
	scanner := bufio.NewScanner(file)
	for i := 0; scanner.Scan(); i++ {
		program = append(program, strings.Split(scanner.Text(), " "))
	}

	problem1(program)
	problem2(program)
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type processor struct {
	registers []int
}

func parseRegister(reg byte) byte {
	return reg - 'a'
}

func parseInt(s string) int {
	value, _ := strconv.Atoi(s)
	return value
}

func (cpu processor) getValue(s string) int {
	if s[0] >= 'a' && s[0] <= 'd' {
		return cpu.registers[s[0]-'a']
	}

	return parseInt(s)
}

func (cpu processor) execute(program [][]string) {
	ip := 0

	for ip < len(program) {
		switch line := program[ip]; line[0] {
		case "cpy":
			cpu.registers[parseRegister(line[2][0])] = cpu.getValue(line[1])
			ip++
		case "inc":
			cpu.registers[parseRegister(line[1][0])]++
			ip++
		case "dec":
			cpu.registers[parseRegister(line[1][0])]--
			ip++
		case "jnz":
			arg := cpu.getValue(line[1])
			if arg != 0 {
				ip += parseInt(line[2])
			} else {
				ip++
			}
		}
	}
}

func problem1(program [][]string) {
	cpu := processor{[]int{0, 0, 0, 0}}
	cpu.execute(program)
	fmt.Println("1:", cpu.registers)
}

func problem2(program [][]string) {
	cpu := processor{[]int{0, 0, 1, 0}}
	cpu.execute(program)
	fmt.Println("2:", cpu.registers)
}
