package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	problem1()
	problem2()
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
		// fmt.Println(ip, cpu, program[ip])
		switch line := program[ip]; line[0] {
		case "tgl":
			arg := cpu.getValue(line[1])
			if ip+arg < len(program) {
				// fmt.Print(">>", ip+arg, program[ip+arg], " -> ")
				program[ip+arg] = toggle(program[ip+arg])
				// fmt.Println(program[ip+arg])
			}
			ip++
		case "cpy":
			if line[2][0] >= 'a' && line[2][0] <= 'd' {
				cpu.registers[parseRegister(line[2][0])] = cpu.getValue(line[1])
			}
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
				ip += cpu.getValue(line[2])
			} else {
				ip++
			}
		case "add":
			/* add is a new instruction added to speed up problem2. Note that input was modified */
			cpu.registers[parseRegister(line[1][0])] += cpu.getValue(line[2])
			ip++
		default:
			fmt.Println("unknown", line)
			panic("error")
		}
	}
}

func toggle(line []string) []string {
	switch line[0] {
	case "inc":
		line[0] = "dec"
	case "dec":
		fallthrough
	case "tgl":
		line[0] = "inc"
	case "cpy":
		line[0] = "jnz"
	case "jnz":
		line[0] = "cpy"

	}

	return line
}

func problem1() {
	file, err := os.Open("input")
	check(err)
	defer file.Close()

	program := [][]string{}
	scanner := bufio.NewScanner(file)
	for i := 0; scanner.Scan(); i++ {
		program = append(program, strings.Split(scanner.Text(), " "))
	}

	cpu := processor{[]int{7, 0, 0, 0}}
	cpu.execute(program)
	fmt.Println("1:", cpu.registers)
}

func problem2() {
	file, err := os.Open("input1")
	check(err)
	defer file.Close()

	program := [][]string{}
	scanner := bufio.NewScanner(file)
	for i := 0; scanner.Scan(); i++ {
		program = append(program, strings.Split(scanner.Text(), " "))
	}

	cpu := processor{[]int{12, 0, 0, 0}}
	cpu.execute(program)
	fmt.Println("2:", cpu.registers)
}
