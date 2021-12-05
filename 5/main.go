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

func Max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func Min(a, b int) int {
	if a <= b {
		return a
	}
	return b
}

type Point struct {
	x int
	y int
}

func PointFromText(s string) Point {
	coords := strings.Split(strings.TrimSpace(s), ",")
	x, err := strconv.Atoi(coords[0])
	if err != nil {
		log.Fatalln(err)
	}

	y, err := strconv.Atoi(coords[1])
	if err != nil {
		log.Fatalln(err)
	}
	return Point{x: x, y: y}
}

type Line struct {
	p1 Point
	p2 Point
}

func LineFromText(s string) Line {
	points := strings.Split(s, "->")
	return Line{
		p1: PointFromText(points[0]),
		p2: PointFromText(points[1]),
	}
}

func (l Line) IsVertical() bool {
	return l.p1.x == l.p2.x
}

func (l Line) IsHorizontal() bool {
	return l.p1.y == l.p2.y
}

func (l Line) Slope() float32 {
	return float32(l.p2.y-l.p1.y) / float32(l.p2.x-l.p1.x)
}

type Grid struct {
	danger map[Point]int
}

func NewGrid() *Grid {
	return &Grid{
		danger: make(map[Point]int),
	}
}

func (g *Grid) AddLine(l Line) {
	if l.IsHorizontal() {
		left := Min(l.p1.x, l.p2.x)
		right := Max(l.p1.x, l.p2.x)
		for i := left; i <= right; i++ {
			p := Point{x: i, y: l.p1.y}
			g.IncDanger(p)
		}
	} else if l.IsVertical() {
		top := Min(l.p1.y, l.p2.y)
		bot := Max(l.p1.y, l.p2.y)
		for i := top; i <= bot; i++ {
			p := Point{x: l.p1.x, y: i}
			g.IncDanger(p)
		}
	} else {
		var xdir int
		var ydir int
		if l.p2.x-l.p1.x < 0 {
			xdir = -1
		} else {
			xdir = 1
		}

		if l.p2.y-l.p1.y < 0 {
			ydir = -1
		} else {
			ydir = 1
		}
		p := l.p1
		g.IncDanger(p)
		for i := 0; i < int(math.Abs(float64(l.p2.x-l.p1.x))); i++ {
			p = Point{x: p.x + xdir, y: p.y + ydir}
			g.IncDanger(p)
		}
	}
}

func (g *Grid) IncDanger(p Point) {
	v, found := g.danger[p]
	if found {
		g.danger[p] = v + 1
	} else {
		g.danger[p] = 1
	}
}

func (g *Grid) Result() int {
	res := 0
	for _, v := range g.danger {
		if v > 1 {
			res += 1
		}
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
	var ls []Line

	for scanner.Scan() {
		ls = append(ls, LineFromText(scanner.Text()))
	}

	g := NewGrid()
	for _, l := range ls {
		g.AddLine(l)
	}
	fmt.Println("Result part 2:", g.Result())
}
