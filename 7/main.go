package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
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
	crabs := ReadIntLine(scanner.Text())

	mf := math.MaxInt
	for i := 0; i < len(crabs); i++ {
		consumed := 0
		for _, o := range crabs {
			diff := math.Abs(float64(o - i))
			if diff != 0 {
				consumed += int(math.Floor((diff * (diff + 1)) / 2))
			}
		}
		mf = int(math.Min(float64(consumed), float64(mf)))
	}

	fmt.Println("Result part 2:", mf)
}
