package main

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"strconv"
	"strings"
)

func main() {
	problem1("cxdnnyjw")
	problem2("cxdnnyjw")
}

func problem1(key string) {
	pwd := ""
	for num := 0; num >= 0; num++ {
		value := key + strconv.Itoa(num)
		h := md5.Sum([]byte(value))
		hash := hex.EncodeToString(h[:])

		if strings.HasPrefix(hash, "00000") {
			pwd = pwd + string(hash[5])
			if len(pwd) == 8 {
				fmt.Println(pwd)
				return
			}
		}
	}
}

func problem2(key string) {
	pwd := make([]byte, 8)
	count := 0
	for num := 0; num >= 0; num++ {
		value := key + strconv.Itoa(num)
		h := md5.Sum([]byte(value))
		hash := hex.EncodeToString(h[:])

		if strings.HasPrefix(hash, "00000") {
			idx := hash[5] - '0'
			if idx < 8 && pwd[idx] == 0 {
				pwd[idx] = hash[6]
				count++
				if count == 8 {
					fmt.Println(string(pwd))
					return
				}
			}
		}
	}
}
