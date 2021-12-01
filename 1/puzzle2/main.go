package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func sum(array []int) int {
	result := 0
	for _, v := range array {
		result += v
	}
	return result
}

func main() {
	f, err := os.Open("puzzle-input.txt")
	if err != nil {
		fmt.Println("Error reading input file.")
		os.Exit(1)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	sws := make([]int, 0)
	for scanner.Scan() {
		l := strings.TrimSpace(scanner.Text())
		r, err := strconv.Atoi(l)
		if err != nil {
			fmt.Println("Error parsing input line", l, "error:", err)
			os.Exit(1)
		}
		sws = append(sws, r)
	}

	pr := -1
	inc := 0
	for i := 0; i < len(sws); i++ {
		cs := sum(sws[i : i+3])
		if pr != -1 && cs > pr {
			inc++
		}
		pr = cs
	}

	fmt.Printf("Number of increases: %d\n", inc)
}
