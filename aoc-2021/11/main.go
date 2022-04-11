package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Octopus struct {
	el int
	i  int
	j  int
	af bool
}

func NewOctopus(i, j, el int) *Octopus {
	return &Octopus{
		i:  i,
		j:  j,
		el: el,
		af: false,
	}
}

func (o *Octopus) Equals(other *Octopus) bool {
	return o.i == other.i && o.j == other.j && o.el == other.el
}

func (o *Octopus) Step() bool {
	if !o.af {
		o.el++
		return o.el > 9
	}
	return false
}

func (o *Octopus) Flash() {
	o.el = 0
	o.af = true
}

func (o *Octopus) Reset() {
	o.af = false
}

type OctopusSet []*Octopus

func (os OctopusSet) Append(o *Octopus) OctopusSet {
	if o.af {
		return os
	}
	for _, so := range os {
		if so.Equals(o) {
			return os
		}
	}
	return append(os, o)
}

func (os OctopusSet) Pop() (*Octopus, OctopusSet) {
	if len(os) == 0 {
		return nil, os
	}
	e := os[0]
	return e, os[1:]
}

type Grid [][]*Octopus

func NewGrid(scanner *bufio.Scanner) Grid {
	g := make(Grid, 0)
	i := 0
	for scanner.Scan() {
		os := make([]*Octopus, 0)
		for j, c := range strings.TrimSpace(scanner.Text()) {
			ci, err := strconv.Atoi(string(c))
			if err != nil {
				log.Fatal(err)
			}
			os = append(os, NewOctopus(i, j, ci))
		}
		g = append(g, os)
		i++
	}
	return g
}

func (g Grid) Print() {
	for _, r := range g {
		for _, o := range r {
			fmt.Printf("%d", o.el)
		}
		fmt.Println()
	}
}

func (g Grid) Step() int {
	pf := make(OctopusSet, 0)
	for _, r := range g {
		for _, o := range r {
			pf = g.SubStep(pf, o)
		}
	}

	flashes := 0
	fo, pf := pf.Pop()
	for fo != nil {
		pf = g.Flash(pf, fo)
		flashes += 1
		fo, pf = pf.Pop()
	}
	g.Reset()
	return flashes
}

func (g Grid) Reset() {
	for _, r := range g {
		for _, o := range r {
			o.Reset()
		}
	}
}
func (g Grid) SubStep(pf OctopusSet, o *Octopus) OctopusSet {
	if o.Step() {
		pf = pf.Append(o)
	}
	return pf
}

func (g Grid) Flash(pf OctopusSet, o *Octopus) OctopusSet {
	o.Flash()
	cu := o.i-1 >= 0
	cd := o.i+1 < len(g)
	cr := o.j+1 < len(g[0])
	cl := o.j-1 >= 0

	if cu {
		pf = g.SubStep(pf, g[o.i-1][o.j])
		if cl {
			pf = g.SubStep(pf, g[o.i-1][o.j-1])
		}
		if cr {
			pf = g.SubStep(pf, g[o.i-1][o.j+1])
		}
	}

	if cd {
		pf = g.SubStep(pf, g[o.i+1][o.j])
		if cr {
			pf = g.SubStep(pf, g[o.i+1][o.j+1])
		}
		if cl {
			pf = g.SubStep(pf, g[o.i+1][o.j-1])
		}
	}

	if cr {
		pf = g.SubStep(pf, g[o.i][o.j+1])
	}

	if cl {
		pf = g.SubStep(pf, g[o.i][o.j-1])
	}
	return pf
}

func Part1(g Grid) (res int) {
	for i := 0; i < 100; i++ {
		res += g.Step()
	}
	return res
}

func AllEqualZero(g Grid) bool {
	for _, r := range g {
		for _, o := range r {
			if o.el != 0 {
				return false
			}
		}
	}
	return true
}

func Part2(g Grid) (i int) {
	for {
		g.Step()
		if AllEqualZero(g) {
			g.Print()
			i++
			return
		} else {
			i++
		}
	}
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(f)
	g := NewGrid(scanner)
	fmt.Println("Part 1 result:", Part1(g))
	fmt.Println("Part 2 result:", Part2(g))
}
