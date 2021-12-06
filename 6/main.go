package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func ReadIntLine(s string) []int {
	var ls []string
	ls = strings.Split(strings.TrimSpace(s), ",")
	var res []int
	for _, ns := range ls {
		in, err := strconv.Atoi(ns)
		if err != nil {
			log.Fatal(err)
		}
		res = append(res, in)
	}
	return res
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	fs := ReadIntLine(scanner.Text())
	count := make([]int, 9)
	for _, f := range fs {
		count[f]++
	}

	for d := 0; d < 256; d++ {
		tmpZ := count[0]
		for t := 1; t < 9; t++ {
			count[t-1] = count[t]
		}
		count[6] += tmpZ
		count[8] = tmpZ
	}

	res := 0
	for _, t := range count {
		res += t
	}

	fmt.Println("Result:", res)
}
