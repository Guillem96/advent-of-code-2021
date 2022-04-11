package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strings"
)

func MinMaxDifference(counter map[rune]int) int {
	maxV := -1
	minV := math.MaxInt
	for _, count := range counter {
		if count > maxV {
			maxV = count
		}
		if count < minV {
			minV = count
		}
	}
	return maxV - minV
}

func MapCopy(dst, src map[string]int) {
	for key, value := range src {
		dst[key] = value
	}
}

type Match struct {
	Pair    string
	Between string
	Pos     int
}

type PolyTemplate string

func (tpl PolyTemplate) ApplyMatches(ms []Match) PolyTemplate {
	ntpl := tpl
	for i, m := range ms {
		pos := m.Pos + i
		ntpl = ntpl[:pos+1] + PolyTemplate(m.Between) + ntpl[pos+1:]
	}
	return ntpl
}

func (tpl PolyTemplate) Result() int {
	counter := make(map[rune]int)
	for _, r := range tpl {
		counter[r]++
	}
	return MinMaxDifference(counter)
}

type Polymerization struct {
	Template PolyTemplate
	PairIR   map[string]string
}

func ReadPolymerization(fpath string) Polymerization {
	f, err := os.Open(fpath)
	if err != nil {
		log.Fatal(err)
	}
	scanner := bufio.NewScanner(f)
	scanner.Scan()
	template := strings.TrimSpace(scanner.Text())

	scanner.Scan()
	pairs := make(map[string]string)
	for scanner.Scan() {
		l := scanner.Text()
		p := strings.Split(l, "->")
		pairs[strings.TrimSpace(p[0])] = strings.TrimSpace(p[1])
	}
	return Polymerization{Template: PolyTemplate(template), PairIR: pairs}
}

func (plm Polymerization) EfficientApplyInsertionRules(steps int) int {
	tpl := plm.Template
	initial := make(map[string]int)
	olp := make(map[string]int)
	for j := 0; j < len(tpl)-1; j++ {
		pair := string(tpl[j : j+2])
		initial[pair]++
		if j > 0 {
			olp[string(tpl[j])]++
		}

		if j+1 < len(tpl)-1 {
			olp[string(tpl[j+1])]++
		}
	}

	current := make(map[string]int)
	MapCopy(current, initial)
	for i := 0; i < steps; i++ {
		nextTpl := make(map[string]int)
		for pair, count := range current {
			btw, found := plm.PairIR[pair]
			if found {
				nextTpl[string(pair[0])+btw] += count
				nextTpl[btw+string(pair[1])] += count
				olp[btw] += count
			} else {
				nextTpl[pair] += count
			}
		}
		current = make(map[string]int)
		MapCopy(current, nextTpl)
	}

	counter := make(map[rune]int)
	for pair, count := range current {
		counter[rune(pair[0])] += count
		counter[rune(pair[1])] += count
	}

	for c, oc := range olp {
		counter[[]rune(c)[0]] -= oc
	}

	return MinMaxDifference(counter)
}

func (plm Polymerization) ApplyInsertionRules(steps int) PolyTemplate {
	tpl := plm.Template
	for i := 0; i < steps; i++ {
		matches := make([]Match, 0)
		for j := 0; j < len(tpl)-1; j++ {
			pair := string(tpl[j : j+2])
			btw, found := plm.PairIR[pair]
			if found {
				matches = append(matches, Match{Pair: pair, Between: btw, Pos: j})
			}
		}
		tpl = tpl.ApplyMatches(matches)
	}

	return tpl
}

func (plm Polymerization) Part1() int {
	tpl := plm.ApplyInsertionRules(10)
	return tpl.Result()
}

func (plm Polymerization) Part2() int {
	return plm.EfficientApplyInsertionRules(40)
}

func main() {
	plm := ReadPolymerization("input.txt")
	fmt.Println("Part 1 result:", plm.Part1())
	fmt.Println("Part 2 result:", plm.Part2())
}
