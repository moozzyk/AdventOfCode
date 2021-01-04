package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
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

	problem(lines)
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type bot struct {
	id   int
	low  int
	high int
}

func (b *bot) giveChip(value int) {
	if b.ready() {
		panic("Already full")
	}

	if b.low == 0 && b.high == 0 {
		b.high = value
	} else {

		var low, high int
		if b.low == 0 {
			low = min(b.high, value)
			high = max(b.high, value)

			fmt.Println(b.id, " compares ", b.high, " and ", value)

		} else {
			low = min(b.low, value)
			high = max(b.low, value)

			fmt.Println(b.id, " compares ", b.low, " and ", value)
		}

		b.low = low
		b.high = high
	}
}

func (b *bot) ready() bool {
	return b.high != 0 && b.low != 0
}

func min(x, y int) int {
	if x < y {
		return x
	}
	return y
}

type instruction struct {
	sourceBotID  int
	targetLow    string
	targetLowID  int
	targetHigh   string
	targetHighID int
}

func max(x, y int) int {
	if x > y {
		return x
	}
	return y
}

var valueGoesExp = regexp.MustCompile(`\d+`)
var botGivesExp = regexp.MustCompile(
	`bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)`)

func problem(lines []string) {
	bots := map[int]*bot{}
	instructions := map[int]instruction{}

	for i := 0; i < len(lines); i++ {
		if strings.HasPrefix(lines[i], "value") {
			match := valueGoesExp.FindAllString(lines[i], -1)
			value, _ := strconv.Atoi(match[0])
			botID, _ := strconv.Atoi(match[1])

			giveToBot(bots, botID, value)
		}
	}

	for i := 0; i < len(lines); i++ {
		if !strings.HasPrefix(lines[i], "value") {

			match := botGivesExp.FindStringSubmatch(lines[i])
			sourceBotID, _ := strconv.Atoi(match[1])
			targetLow := match[2]
			targetIDLow, _ := strconv.Atoi(match[3])
			targetHigh := match[4]
			targetIDHigh, _ := strconv.Atoi(match[5])
			instructions[sourceBotID] = instruction{sourceBotID, targetLow, targetIDLow, targetHigh, targetIDHigh}
		}
	}

	execute(bots, instructions)
}

func execute(bots map[int]*bot, instructions map[int]instruction) {
	outputBins := map[int][]int{}

	for b := findReadyBot(bots); b != nil; b = findReadyBot(bots) {
		instr, ok := instructions[b.id]
		if !ok {
			panic("No instructions for the bot")
		}

		if instr.targetLow == "bot" {
			giveToBot(bots, instr.targetLowID, b.low)
		} else {
			giveToOuput(outputBins, instr.targetLowID, b.low)
		}

		if instr.targetHigh == "bot" {
			giveToBot(bots, instr.targetHighID, b.high)
		} else {
			giveToOuput(outputBins, instr.targetHighID, b.high)
		}

		b.low = 0
		b.high = 0
		delete(bots, b.id)
		delete(instructions, b.id)
	}

	fmt.Println("Problem 2: ", outputBins[0][0]*outputBins[1][0]*outputBins[2][0])
}

func findReadyBot(bots map[int]*bot) *bot {
	for _, b := range bots {
		if b.ready() == true {
			return b
		}
	}

	return nil
}

func giveToBot(bots map[int]*bot, botID int, value int) {
	b, ok := bots[botID]

	if !ok {
		b = &bot{botID, 0, 0} // math.MaxInt32, math.MinInt32}
		bots[botID] = b
	}

	b.giveChip(value)
}

func giveToOuput(outputBins map[int][]int, binID, value int) {
	if _, ok := outputBins[binID]; !ok {
		outputBins[binID] = []int{value}
	} else {
		outputBins[binID] = append(outputBins[binID], value)
	}
}
