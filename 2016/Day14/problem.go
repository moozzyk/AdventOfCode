package main

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"strconv"
	"strings"
)

func main() {
	problem1("zpqevtbw")
	problem2("zpqevtbw")
}

func problem1(seed string) {
	i := 0
	for count := 0; count < 64; i++ {
		hash := getHash(seed, i)

		if c := find3Same(hash); c != 0 {
			key := strings.Repeat(string(c), 5)
			for j := i + 1; j < i+1001; j++ {
				verify := getHash(seed, j)
				if strings.Contains(verify, key) {
					count++
					fmt.Println(count, i, j, hash, verify)
					break
				}
			}
		}
	}
	fmt.Println(i - 1)
}

func problem2(seed string) {
	i := 0
	for count := 0; count < 64; i++ {
		hash := getHashManyTimes(seed, i)

		if c := find3Same(hash); c != 0 {
			key := strings.Repeat(string(c), 5)
			for j := i + 1; j < i+1001; j++ {
				verify := getHashManyTimes(seed, j)
				if strings.Contains(verify, key) {
					count++
					fmt.Println(count, i, j, hash, verify)
					break
				}
			}
		}
	}
	fmt.Println(i - 1)
}

func getHash(seed string, n int) string {
	val := seed + strconv.Itoa(n)
	hash := md5.Sum([]byte(val))
	return hex.EncodeToString(hash[:])
}

var hashcache = map[string]string{}

func getHashManyTimes(seed string, n int) string {

	val := seed + strconv.Itoa(n)
	originalVal := val
	if h, ok := hashcache[val]; ok {
		return h
	}

	hash := md5.Sum([]byte(val))
	for i := 0; i < 2016; i++ {
		val = hex.EncodeToString(hash[:])
		hash = md5.Sum([]byte(val))
	}
	hashcache[originalVal] = hex.EncodeToString(hash[:])
	return hashcache[originalVal]
}

func find3Same(value string) byte {
	for i := 0; i < len(value)-2; i++ {
		if value[i] == value[i+1] && value[i+1] == value[i+2] {
			return value[i]
		}
	}
	return 0
}

func find5Same(value string) []string {
	res := []string{}
	for i := 0; i < len(value)-4; i++ {
		if value[i] == value[i+1] && value[i+1] == value[i+2] && value[i+2] == value[i+3] && value[i+3] == value[i+4] {
			res = append(res, strings.Repeat(string(value[i]), 5))
			i += 4
		}
	}

	return res
}

/*
This was supposed to be a smarter way that is not working and apparently not needed
func problem1(seed string) {

	locations := map[string][]int{}

	i := 0
	for count := 0; count < 64 && i < 22728; i++ {
		hash := getHash(seed, i)

		if c := find3Same(hash); c != 0 {
			key := strings.Repeat(string(c), 5)
			if _, ok := locations[key]; !ok {
				locations[key] = []int{}
			}
			locations[key] = append(locations[key], i)

			fmt.Println(i, hash, len(locations))
		}

		pentets := find5Same(hash)
		for j := 0; j < len(pentets); j++ {
			if locs, ok := locations[pentets[j]]; ok {
				for k := 0; k < len(locs)-1; k++ {
					if locs[k] < i && locs[k] > i-1000 {
						count++
						locs = append(locs[0:k], locs[k+1:]...)
						fmt.Println(k, locs, pentets, pentets[j])
						break
					}
				}
			}
		}
	}

	fmt.Println(locations)
	fmt.Println(i)
}
*/
