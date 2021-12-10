package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

func ReadLine(s string) []int {
	l := strings.TrimSpace(s)
	var res []int
	for _, n := range l {
		ni, err := strconv.Atoi(string(n))
		if err != nil {
			log.Fatal(err)
		}
		res = append(res, ni)
	}
	return res
}

func ReadGrid(scanner *bufio.Scanner) [][]int {
	var grid [][]int
	for scanner.Scan() {
		grid = append(grid, ReadLine(scanner.Text()))
	}
	return grid
}

func Part1(g [][]int) int {
	prob := 0
	for i := 0; i < len(g); i++ {
		for j := 0; j < len(g[i]); j++ {
			if g[i][j] == 9 {
				continue
			}
			c := g[i][j]
			itl := true
			if i > 0 {
				itl = g[i-1][j] > c
			}

			ibl := true
			if i < len(g)-1 {
				ibl = g[i+1][j] > c
			}

			ill := true
			if j > 0 {
				ill = g[i][j-1] > c
			}

			irl := true
			if j < len(g[i])-1 {
				irl = g[i][j+1] > c
			}

			if itl && ibl && irl && ill {
				prob += c + 1
			}
		}
	}
	return prob
}

type Point struct {
	i     int
	j     int
	value int
}

type Basin struct {
	Length int
	points map[Point]bool
}

type Basins []*Basin

func (bs Basins) Contains(p Point) bool {
	for _, b := range bs {
		if b.Contains(p) {
			return true
		}
	}
	return false
}

func NewBasin() *Basin {
	ps := make(map[Point]bool)
	return &Basin{
		Length: 0,
		points: ps,
	}
}

func (b *Basin) Add(p Point) {
	_, found := b.points[p]
	if !found {
		b.points[p] = true
		b.Length++
	}
}

func (b *Basin) Contains(p Point) bool {
	_, found := b.points[p]
	return found
}

func VisitPoint(i, j int, g [][]int, b *Basin) {
	if i >= len(g) || i < 0 || j >= len(g[i]) || j < 0 {
		return
	}

	v := g[i][j]
	if v == 9 {
		return
	}

	p := Point{i: i, j: j, value: g[i][j]}
	if b.Contains(p) {
		return
	}

	b.Add(Point{i: i, j: j, value: g[i][j]})
	VisitPoint(i+1, j, g, b)
	VisitPoint(i, j+1, g, b)
	VisitPoint(i-1, j, g, b)
	VisitPoint(i, j-1, g, b)
}

func Part2(g [][]int) int {
	var bs Basins
	for i := 0; i < len(g); i++ {
		for j := 0; j < len(g[i]); j++ {
			p := Point{i: i, j: j, value: g[i][j]}
			if p.value != 9 && !bs.Contains(p) {
				b := NewBasin()
				bs = append(bs, b)
				VisitPoint(i, j, g, b)
			}
		}
	}

	sort.Slice(bs, func(i, j int) bool {
		return bs[i].Length > bs[j].Length
	})

	res := 1
	for _, b := range bs[:3] {
		fmt.Println(b.Length)
		res *= b.Length
	}

	return res
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	g := ReadGrid(bufio.NewScanner(f))
	fmt.Println("Part 1 result:", Part1(g))
	fmt.Println("Part 2 result:", Part2(g))
}
