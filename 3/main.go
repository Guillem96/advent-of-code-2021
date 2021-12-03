package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func Puzzle1(bs []string) int {
	nb := len(bs[0])
	nr := len(bs)
	var gr uint
	var er uint
	for col := 0; col < nb; col++ {
		sum := 0
		for _, r := range bs {
			if r[col] == '1' {
				sum += 1
			}
		}

		if sum > nr/2 {
			gr += (1 << (nb - col - 1))
		} else {
			er += (1 << (nb - col - 1))
		}
	}
	return int(gr * er)
}

func Bin2Int(binary string) int {
	nb := len(binary)
	var res int
	for i, b := range binary {
		if b == '1' {
			res += (1 << (nb - i - 1))
		}
	}
	return res
}

func MostCommon(bs []string, bp int) ([]string, []string) {
	ones := make([]string, 0)
	zeros := make([]string, 0)

	for _, r := range bs {
		if r[bp] == '1' {
			ones = append(ones, r)
		} else {
			zeros = append(zeros, r)
		}
	}

	if len(ones) >= len(zeros) {
		return ones, zeros
	} else {
		return zeros, ones
	}
}

func FindRates(o2GenRating, c2ScrubberRaring []string, bp int) ([]string, []string) {
	var o2res []string
	var c2res []string

	if len(o2GenRating) == 1 && len(c2ScrubberRaring) == 1 {
		return o2GenRating, c2ScrubberRaring
	}

	if len(o2GenRating) == 1 {
		o2res = o2GenRating
	} else {
		o2res, _ = MostCommon(o2GenRating, bp)
	}

	if len(c2ScrubberRaring) == 1 {
		c2res = c2ScrubberRaring
	} else {
		_, c2res = MostCommon(c2ScrubberRaring, bp)
	}

	return FindRates(o2res, c2res, bp+1)
}

func Puzzle2(bs []string) int {
	o2, c2 := MostCommon(bs, 0)
	o2, c2 = FindRates(o2, c2, 1)
	return Bin2Int(o2[0]) * Bin2Int(c2[0])
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error reading input file.")
		os.Exit(1)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	binaries := make([]string, 0)
	for scanner.Scan() {
		l := scanner.Text()
		binaries = append(binaries, strings.TrimSpace(l))
	}

	fmt.Println("Result Puzzle 1:", Puzzle1(binaries))
	fmt.Println("Result Puzzle 2:", Puzzle2(binaries))
}
