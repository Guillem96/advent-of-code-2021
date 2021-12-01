package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func isIncrease(line string, previousValue int) (bool, int) {
	l := strings.TrimSpace(line)
	r, err := strconv.Atoi(l)
	if err != nil {
		fmt.Println("Error parsing input line", line, "error:", err)
		os.Exit(1)
	}
	return r > previousValue && previousValue != -1, r
}

func main() {
	f, err := os.Open("puzzle-input.txt")
	if err != nil {
		fmt.Println("Error reading input file.")
		os.Exit(1)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	pr := -1
	inc := 0

	for scanner.Scan() {
		isInc, cpr := isIncrease(scanner.Text(), pr)
		pr = cpr
		if isInc {
			inc++
		}
	}

	fmt.Printf("Number of increases: %d\n", inc)
}
