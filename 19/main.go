package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Cube struct {
	Xmin int
	Xmax int
	Ymin int
	Ymax int
	Zmin int
	Zmax int
}

func (c Cube) Overlap(o Cube) bool {
	if c.Xmin > o.Xmax {
		return false
	}
	if c.Xmax < o.Xmin {
		return false
	}
	if c.Ymin > o.Ymax {
		return false
	}
	if c.Ymax < o.Ymin {
		return false
	}
	if c.Zmin > o.Zmax {
		return false
	}
	if c.Zmax < o.Zmin {
		return false
	}
	return true
	// return ((c.Xmin <= o.Xmin && o.Xmin <= c.Xmax) || (o.Xmin <= c.Xmin && c.Xmin <= o.Xmax)) &&
	// 	((c.Ymin <= o.Ymin && o.Ymin <= c.Ymax) || (o.Ymin <= c.Ymin && c.Ymin <= o.Ymax)) &&
	// 	((c.Zmin <= o.Zmin && o.Zmin <= c.Zmax) || (o.Zmin <= c.Zmin && c.Zmin <= o.Zmax))
}

func (c Cube) Interpretations() []Cube {
	return []Cube{
		{Xmin: c.Xmin, Xmax: c.Xmax, Ymin: c.Ymin, Ymax: c.Ymax, Zmin: c.Zmin, Zmax: c.Zmax},
		{Xmin: -c.Xmin, Xmax: -c.Xmax, Ymin: c.Ymin, Ymax: c.Ymax, Zmin: c.Zmin, Zmax: c.Zmax},
		{Xmin: c.Xmin, Xmax: c.Xmax, Ymin: -c.Ymin, Ymax: -c.Ymax, Zmin: c.Zmin, Zmax: c.Zmax},
		{Xmin: c.Xmin, Xmax: c.Xmax, Ymin: c.Ymin, Ymax: c.Ymax, Zmin: -c.Zmin, Zmax: -c.Zmax},
		{Xmin: -c.Xmin, Xmax: -c.Xmax, Ymin: c.Ymin, Ymax: c.Ymax, Zmin: -c.Zmin, Zmax: -c.Zmax},
		{Xmin: -c.Xmin, Xmax: -c.Xmax, Ymin: -c.Ymin, Ymax: -c.Ymax, Zmin: c.Zmin, Zmax: c.Zmax},
		{Xmin: c.Xmin, Xmax: c.Xmax, Ymin: -c.Ymin, Ymax: -c.Ymax, Zmin: -c.Zmin, Zmax: -c.Zmax},
		{Xmin: -c.Xmin, Xmax: -c.Xmax, Ymin: -c.Ymin, Ymax: -c.Ymax, Zmin: -c.Zmin, Zmax: -c.Zmax},

		{Xmin: c.Zmin, Xmax: c.Zmax, Ymin: c.Ymin, Ymax: c.Ymax, Zmin: c.Xmin, Zmax: c.Xmax},
		{Xmin: -c.Zmin, Xmax: -c.Zmax, Ymin: c.Ymin, Ymax: c.Ymax, Zmin: c.Xmin, Zmax: c.Xmax},
		{Xmin: c.Zmin, Xmax: c.Zmax, Ymin: -c.Ymin, Ymax: -c.Ymax, Zmin: c.Xmin, Zmax: c.Xmax},
		{Xmin: c.Zmin, Xmax: c.Zmax, Ymin: c.Ymin, Ymax: c.Ymax, Zmin: -c.Xmin, Zmax: -c.Xmax},
		{Xmin: -c.Zmin, Xmax: -c.Zmax, Ymin: c.Ymin, Ymax: c.Ymax, Zmin: -c.Xmin, Zmax: -c.Xmax},
		{Xmin: -c.Zmin, Xmax: -c.Zmax, Ymin: -c.Ymin, Ymax: -c.Ymax, Zmin: c.Xmin, Zmax: c.Xmax},
		{Xmin: c.Zmin, Xmax: c.Zmax, Ymin: -c.Ymin, Ymax: -c.Ymax, Zmin: -c.Xmin, Zmax: -c.Xmax},
		{Xmin: -c.Zmin, Xmax: -c.Zmax, Ymin: -c.Ymin, Ymax: -c.Ymax, Zmin: -c.Xmin, Zmax: -c.Xmax},

		{Xmin: c.Ymin, Xmax: c.Ymax, Ymin: c.Xmin, Ymax: c.Xmax, Zmin: c.Zmin, Zmax: c.Zmax},
		{Xmin: -c.Ymin, Xmax: -c.Ymax, Ymin: c.Xmin, Ymax: c.Xmax, Zmin: c.Zmin, Zmax: c.Zmax},
		{Xmin: c.Ymin, Xmax: c.Ymax, Ymin: -c.Xmin, Ymax: -c.Xmax, Zmin: c.Zmin, Zmax: c.Zmax},
		{Xmin: c.Ymin, Xmax: c.Ymax, Ymin: c.Xmin, Ymax: c.Xmax, Zmin: -c.Zmin, Zmax: -c.Zmax},
		{Xmin: -c.Ymin, Xmax: -c.Ymax, Ymin: c.Xmin, Ymax: c.Xmax, Zmin: -c.Zmin, Zmax: -c.Zmax},
		{Xmin: -c.Ymin, Xmax: -c.Ymax, Ymin: -c.Xmin, Ymax: -c.Xmax, Zmin: c.Zmin, Zmax: c.Zmax},
		{Xmin: c.Ymin, Xmax: c.Ymax, Ymin: -c.Xmin, Ymax: -c.Xmax, Zmin: -c.Zmin, Zmax: -c.Zmax},
		{Xmin: -c.Ymin, Xmax: -c.Ymax, Ymin: -c.Xmin, Ymax: -c.Xmax, Zmin: -c.Zmin, Zmax: -c.Zmax},
	}
}

type Point struct {
	X int
	Y int
	Z int
}

func (p *Point) Interpretaions() []Point {
	return []Point{
		{X: p.X, Y: p.Y, Z: p.Z},
		{X: p.Y, Y: p.Z, Z: p.X},
		{X: p.Z, Y: p.X, Z: p.Y},
		{X: -p.X, Y: p.Z, Z: p.Y},
		{X: p.Z, Y: p.Y, Z: -p.X},
		{X: p.Y, Y: -p.X, Z: p.Z},
		{X: p.X, Y: p.Z, Z: -p.Y},
		{X: p.Z, Y: -p.Y, Z: p.X},
		{X: -p.Y, Y: p.X, Z: p.Z},
		{X: p.X, Y: -p.Z, Z: p.Y},
		{X: -p.Z, Y: p.Y, Z: p.X},
		{X: p.Y, Y: p.X, Z: -p.Z},
		{X: -p.X, Y: -p.Y, Z: p.Z},
		{X: -p.Y, Y: p.Z, Z: -p.X},
		{X: p.Z, Y: -p.X, Z: -p.Y},
		{X: -p.X, Y: p.Y, Z: -p.Z},
		{X: p.Y, Y: -p.Z, Z: -p.X},
		{X: -p.Z, Y: -p.X, Z: p.Y},
		{X: p.X, Y: -p.Y, Z: -p.Z},
		{X: -p.Y, Y: -p.Z, Z: p.X},
		{X: -p.Z, Y: p.X, Z: -p.Y},
		{X: -p.X, Y: -p.Z, Z: -p.Y},
		{X: -p.Z, Y: -p.Y, Z: -p.X},
		{X: -p.Y, Y: -p.X, Z: -p.Z},
	}
}

func PointFromText(text string) Point {
	text = strings.TrimSpace(text)
	coords := strings.Split(text, ",")
	x, err := strconv.Atoi(coords[0])
	if err != nil {
		log.Fatal(err)
	}
	y, err := strconv.Atoi(coords[1])
	if err != nil {
		log.Fatal(err)
	}
	z, err := strconv.Atoi(coords[2])
	if err != nil {
		log.Fatal(err)
	}
	return Point{X: x, Y: y, Z: z}
}

type Scanner struct {
	Position      Point
	Beacons       []Point
	KnownPosition bool
}

func (s Scanner) Cubes() []Cube {
	xmin := 1001
	xmax := -1000
	ymin := 1001
	ymax := -1000
	zmin := 1001
	zmax := -1000
	for _, p := range s.Beacons {
		if p.X > xmax {
			xmax = p.X
		}
		if p.X < xmin {
			xmin = p.X
		}

		if p.Y > ymax {
			ymax = p.Y
		}
		if p.Y < ymin {
			ymin = p.Y
		}

		if p.Z > zmax {
			zmax = p.Z
		}
		if p.Z < zmin {
			zmin = p.Z
		}
	}
	c := Cube{Xmin: xmin, Xmax: xmax, Ymin: ymin, Ymax: ymax, Zmin: zmin, Zmax: zmax}
	return c.Interpretations()
}

func (s Scanner) Overlap(o Scanner) bool {
	n := 0
	for _, sp := range s.Beacons {
		for _, op := range o.Beacons {
			for _, spi := range sp.Interpretaions() {
				for _, opi := range op.Interpretaions() {
					// Xb = o.Position.X + opi.X
					// Xb = sp.Position.X + spi.X
					// Yb = o.Position.Y + opi.Y
					// Yb = sp.Position.Y + spi.Y
					// Zb = o.Position.Z + opi.Z
					// Zb = sp.Position.Z + spi.Z
				}
			}
		}
	}
	return n >= 12
}

func ReadScannerReads(fpath string) []Scanner {
	f, err := os.Open(fpath)
	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(f)
	srs := make([]Scanner, 0)
	currentScanner := 0
	for scanner.Scan() {
		l := scanner.Text()
		if l == "" {
			currentScanner++
		} else if l[:3] == "---" {
			if currentScanner == 0 {
				ns := Scanner{
					Beacons:       make([]Point, 0),
					Position:      Point{X: 0, Y: 0, Z: 0},
					KnownPosition: true,
				}
				srs = append(srs, ns)
			} else {
				ns := Scanner{
					Beacons:       make([]Point, 0),
					KnownPosition: false,
				}
				srs = append(srs, ns)
			}
		} else {
			srs[currentScanner].Beacons = append(srs[currentScanner].Beacons, PointFromText(scanner.Text()))
		}
	}
	return srs
}

func main() {
	srs := ReadScannerReads("test.txt")

	for i, o := range srs[1:] {
		if srs[0].Overlap(o) {
			fmt.Println("Scanner 0 overlaps with scanner", i+1)
		}
	}
}
