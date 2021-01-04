package main

import "fmt"

func main() {
	input := []byte{1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0}
	problem(input, 272)
	problem(input, 35651584)
}

func problem(data []byte, size int) {

	for len(data) < size {
		cp := make([]byte, len(data))
		for i := 0; i < len(cp); i++ {
			cp[i] = (^data[len(data)-i-1]) & 1
		}
		data = append(append(data, 0), cp...)
	}

	checkSum := calculateCheckSum(data[:size])
	fmt.Print("checkSum: ")
	for i := 0; i < len(checkSum); i++ {
		if checkSum[i] == 0 {
			fmt.Print(0)
		} else {
			fmt.Print(1)
		}
	}
	fmt.Println()
}

func calculateCheckSum(data []byte) []byte {
	if (len(data) & 1) != 0 {
		return data
	}

	checkSum := make([]byte, len(data)>>1)

	for i := 0; i < len(checkSum); i++ {
		checkSum[i] = (^(data[i*2] ^ data[i*2+1])) & 1
		// fmt.Print(checkSum[i])
	}
	// fmt.Println()
	return calculateCheckSum(checkSum)
}
