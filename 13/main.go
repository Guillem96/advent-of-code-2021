package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

const (
	Up   = "up"
	Left = "left"
)

type FoldInstruction struct {
	Way   string
	Value int
}

func ReadFoldInstruction(line string) FoldInstruction {
	l := strings.TrimSpace(line)
	var fi []string
	var way string
	if strings.Contains(l, "x=") {
		fi = strings.Split(l, "x=")
		way = Left
	} else {
		fi = strings.Split(l, "y=")
		way = Up
	}

	v, err := strconv.Atoi(fi[1])
	if err != nil {
		log.Fatal(err)
	}
	return FoldInstruction{Way: way, Value: v}
}

type Point struct {
	X int
	Y int
}

func ReadPoint(line string) Point {
	p := strings.Split(strings.TrimSpace(line), ",")

	x, err := strconv.Atoi(p[0])
	if err != nil {
		log.Fatal(err)
	}

	y, err := strconv.Atoi(p[1])
	if err != nil {
		log.Fatal(err)
	}

	return Point{X: x, Y: y}
}

func (p Point) ApplyFold(fi FoldInstruction) Point {
	if fi.Way == Up && p.Y > fi.Value {
		dist := p.Y - fi.Value
		newY := fi.Value - dist
		return Point{X: p.X, Y: newY}
	} else if fi.Way == Left && p.X > fi.Value {
		dist := p.X - fi.Value
		newX := fi.Value - dist
		return Point{X: newX, Y: p.Y}
	} else {
		return p
	}
}

type TransparentPaper struct {
	Points  map[Point]bool
	FoldIns []FoldInstruction
}

func ReadTransparentPaper(scanner *bufio.Scanner) TransparentPaper {
	rp := true
	ps := make(map[Point]bool)
	var fis []FoldInstruction
	for scanner.Scan() {
		l := scanner.Text()
		if l == "" {
			rp = false
		} else if rp {
			p := ReadPoint(l)
			ps[p] = true
		} else {
			fis = append(fis, ReadFoldInstruction(l))
		}
	}
	return TransparentPaper{Points: ps, FoldIns: fis}
}

func (tp TransparentPaper) ApplyFolds(limit int) TransparentPaper {
	if limit == -1 {
		limit = len(tp.FoldIns)
	}

	ntp := tp
	for i := 0; i < limit; i++ {
		nps := make(map[Point]bool)
		fi := tp.FoldIns[i]
		for p := range ntp.Points {
			nps[p.ApplyFold(fi)] = true
		}
		ntp = TransparentPaper{Points: nps, FoldIns: tp.FoldIns[limit:]}
	}
	return ntp
}

func (tp TransparentPaper) Print() {
	maxX := -1
	maxY := -1
	for p := range tp.Points {
		if p.X > maxX {
			maxX = p.X
		}
		if p.Y > maxY {
			maxY = p.Y
		}
	}

	for y := 0; y <= maxY; y++ {
		for x := 0; x <= maxX; x++ {
			p := Point{X: x, Y: y}
			_, found := tp.Points[p]
			if found {
				fmt.Printf("#")
			} else {
				fmt.Printf(".")
			}
		}
		fmt.Println()
	}
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(f)
	tp := ReadTransparentPaper(scanner)
	part1tp := tp.ApplyFolds(1)
	part2tp := tp.ApplyFolds(-1)
	fmt.Printf("Part 1 result: %d\n", len(part1tp.Points))

	fmt.Printf("*** Part 2 ***\n")
	part2tp.Print()
}
