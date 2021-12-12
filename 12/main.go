package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
	"unicode"
)

type Cave string

func (c Cave) IsSmall() bool {
	if c == "start" || c == "end" {
		return false
	}

	for _, r := range c {
		if unicode.IsUpper(r) {
			return false
		}
	}
	return true
}

type CaveSystem map[Cave][]Cave

func (cs CaveSystem) GetValidChildren(c Cave, p Path) []Cave {
	children := cs[c]
	var vc []Cave
	for _, child := range children {
		if p.CanGoToPart2(child) {
			vc = append(vc, child)
		}
	}
	return vc
}

type Node struct {
	c Cave
	p Path
}

func (cs CaveSystem) FindPaths() []Path {
	var queue []Node
	var paths []Path
	sp := make(Path, 1)
	sp[0] = Cave("start")
	queue = append(queue, Node{c: Cave("start"), p: sp})
	for len(queue) != 0 {
		cn := queue[0]
		queue = queue[1:]
		if cn.c == Cave("end") {
			paths = append(paths, cn.p)
		} else {
			children := cs.GetValidChildren(cn.c, cn.p)
			for _, c := range children {
				np := make(Path, len(cn.p))
				copy(np, cn.p)
				np = append(np, c)
				queue = append(queue, Node{c: c, p: np})
			}
		}
	}
	return paths
}

type Path []Cave

func (p Path) CanGoToPart1(c Cave) bool {
	if !c.IsSmall() {
		return true
	}
	for _, sp := range p {
		if sp == c {
			return false
		}
	}
	return true
}

func (p Path) CanGoToPart2(c Cave) bool {
	if !c.IsSmall() {
		return true
	}

	counter := make(map[Cave]int)
	for _, pc := range p {
		if pc.IsSmall() {
			counter[pc]++
		}
	}

	v, found := counter[c]
	if !found {
		// Never visited so we can go to it
		return true
	} else {
		if v == 2 {
			// Cannot be visited more
			return false
		}
		// Check if it can be visited twice
		for _, visits := range counter {
			if visits == 2 {
				return false
			}
		}
		return true
	}
}

func ReadCaveSystem(fpath string) CaveSystem {
	f, err := os.Open(fpath)
	if err != nil {
		log.Fatal(err)
	}
	cs := make(CaveSystem)
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		l := strings.TrimSpace(scanner.Text())
		se := strings.Split(l, "-")
		sc := Cave(se[0])
		ec := Cave(se[1])

		if ec != "start" && sc != "end" {
			_, found := cs[sc]
			if !found {
				cs[sc] = make([]Cave, 0)
			}
			cs[sc] = append(cs[sc], ec)
		}

		if sc != "start" && ec != "end" {
			_, found := cs[ec]
			if !found {
				cs[ec] = make([]Cave, 0)
			}
			cs[ec] = append(cs[ec], sc)
		}
	}
	return cs
}

func main() {
	cs := ReadCaveSystem("input.txt")
	paths := cs.FindPaths()
	fmt.Printf("Part 2 result: %d\n", len(paths))
}
