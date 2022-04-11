package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strings"
)

type StringSet string

func (ss StringSet) Equals(o StringSet) bool {
	if len(ss) != len(o) {
		return false
	}

	for _, s := range ss {
		if !strings.Contains(string(o), string(s)) {
			return false
		}
	}
	return true
}

func (ss StringSet) Difference(o StringSet) StringSet {
	m2 := make(map[string]bool)
	var diff string
	for _, item := range o {
		m2[string(item)] = true
	}

	for _, item := range ss {
		if _, ok := m2[string(item)]; !ok {
			diff += string(item)
		}
	}

	return StringSet(diff)
}

type Entry struct {
	input  []StringSet
	output []StringSet
}

func EntryFromLine(s string) Entry {
	var inputss []StringSet
	var outputss []StringSet

	entry := strings.Split(s, "|")
	input := strings.Split(strings.TrimSpace(entry[0]), " ")
	for _, i := range input {
		inputss = append(inputss, StringSet(i))
	}
	output := strings.Split(strings.TrimSpace(entry[1]), " ")
	for _, o := range output {
		outputss = append(outputss, StringSet(o))
	}

	return Entry{input: inputss, output: outputss}
}

func (e Entry) Decode() int {

	// Store here decoded digits
	var decoded [10]StringSet

	for _, inp := range e.input {
		if len(inp) == 2 {
			decoded[1] = inp
		} else if len(inp) == 3 {
			decoded[7] = inp
		} else if len(inp) == 4 {
			decoded[4] = inp
		} else if len(inp) == 7 {
			decoded[8] = inp
		}
	}

	for _, inp := range e.input {
		if len(inp) == 5 { // 2, 5, or 3
			if len(inp.Difference(decoded[1])) == 3 {
				decoded[3] = inp
			} else if len(inp.Difference(decoded[4])) == 2 {
				decoded[5] = inp
			} else if len(inp.Difference(decoded[4])) == 3 {
				decoded[2] = inp
			}
		} else if len(inp) == 6 { // 6, 9 or 0
			if len(inp.Difference(decoded[1])) == 5 {
				decoded[6] = inp
			} else if len(inp.Difference(decoded[4])) == 2 {
				decoded[9] = inp
			} else if len(inp.Difference(decoded[4])) == 3 {
				decoded[0] = inp
			}
		}
	}

	res := 0
	for i, o := range e.output {
		for j, d := range decoded {
			if o.Equals(d) {
				res += int(math.Pow(10, float64(4-i-1))) * j
			}
		}
	}

	return res
}

func Part1(es []Entry) int {
	counter := 0
	for _, e := range es {
		for _, o := range e.output {
			if len(o) == 2 || len(o) == 4 || len(o) == 7 || len(o) == 3 {
				counter++
			}
		}
	}
	return counter
}

func Part2(es []Entry) int {
	result := 0
	for _, e := range es {
		result += e.Decode()
	}
	return result
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	var es []Entry
	for scanner.Scan() {
		es = append(es, EntryFromLine(scanner.Text()))
	}

	p1r := Part1(es)
	p2r := Part2(es)

	fmt.Println("Result part 1:", p1r)
	fmt.Println("Result part 2:", p2r)
}
