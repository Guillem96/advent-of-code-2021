package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

type Cube struct {
	X int
	Y int
	Z int
}

type Range struct {
	Min int
	Max int
}

func RangeFromText(txt string) Range {
	txt = strings.TrimSpace(txt[2:])
	minmax := strings.Split(txt, "..")
	min, err := strconv.Atoi(minmax[0])
	if err != nil {
		log.Fatal(err)
	}
	max, err := strconv.Atoi(minmax[1])
	if err != nil {
		log.Fatal(err)
	}
	return Range{Min: min, Max: max + 1}
}

type Step struct {
	On     bool
	Xrange Range
	Yrange Range
	Zrange Range
}

func StepFromText(txt string) Step {
	parts := strings.Split(txt, " ")
	var onoff bool
	if parts[0] == "on" {
		onoff = true
	} else {
		onoff = false
	}

	ranges := strings.Split(parts[1], ",")

	return Step{
		On:     onoff,
		Xrange: RangeFromText(ranges[0]),
		Yrange: RangeFromText(ranges[1]),
		Zrange: RangeFromText(ranges[2]),
	}
}

func ReadInstructions(fpath string) []Step {
	f, err := os.Open(fpath)
	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(f)
	var steps []Step
	for scanner.Scan() {
		steps = append(steps, StepFromText(scanner.Text()))
	}
	return steps
}

type Reactor map[Cube]bool

func Part1(steps []Step) (co int) {
	// oncs := make([]Cuboid, 0)
	r := make(Reactor)
	for _, s := range steps {
		if s.Xrange.Min > 50 {
			continue
		}
		if s.Xrange.Max < -50 {
			continue
		}

		if s.Yrange.Min > 50 {
			continue
		}
		if s.Yrange.Max < -50 {
			continue
		}

		if s.Zrange.Min > 50 {
			continue
		}
		if s.Zrange.Max < -50 {
			continue
		}

		for x := s.Xrange.Min; x < s.Xrange.Max; x++ {
			for y := s.Yrange.Min; y < s.Yrange.Max; y++ {
				for z := s.Zrange.Min; z < s.Zrange.Max; z++ {
					r[Cube{X: x, Y: y, Z: z}] = s.On
				}
			}
		}
	}

	for _, v := range r {
		if v {
			co++
		}
	}
	return co
}

type Cuboid struct {
	Xmin int
	Xmax int
	Ymin int
	Ymax int
	Zmin int
	Zmax int
}

func CuboidFromStep(s Step) Cuboid {
	return Cuboid{
		Xmin: s.Xrange.Min,
		Xmax: s.Xrange.Max,
		Ymin: s.Yrange.Min,
		Ymax: s.Yrange.Max,
		Zmin: s.Zrange.Min,
		Zmax: s.Zrange.Max,
	}
}

func (c Cuboid) Volume() int {
	return (c.Xmax - c.Xmin) * (c.Ymax - c.Ymin) * (c.Zmax - c.Zmin)
}

func (c Cuboid) Overlaps(o Cuboid) bool {
	if c.Xmax < o.Xmin {
		return false
	}
	if o.Xmax < c.Xmin {
		return false
	}
	if c.Ymax < o.Ymin {
		return false
	}
	if o.Ymax < c.Ymin {
		return false
	}
	if c.Zmax < o.Zmin {
		return false
	}
	if o.Zmax < c.Zmin {
		return false
	}
	return true

}

func (c Cuboid) Contains(o Cuboid) bool {
	return c.Xmin <= o.Xmin && c.Xmax >= o.Xmax &&
		c.Ymin <= o.Ymin && c.Ymax >= o.Ymax &&
		c.Zmin <= o.Zmin && c.Zmax >= o.Zmax
}

func (c Cuboid) Subtract(o Cuboid) []Cuboid {
	cs := make([]Cuboid, 0)
	if o.Contains(c) {
		return cs
	}

	if !c.Overlaps(o) {
		return []Cuboid{c}
	}

	var xs []int
	xs = append(xs, c.Xmin)
	if c.Xmin < o.Xmin && o.Xmin < c.Xmax {
		xs = append(xs, o.Xmin)
	}
	if c.Xmin < o.Xmax && o.Xmax < c.Xmax {
		xs = append(xs, o.Xmax)
	}
	xs = append(xs, c.Xmax)

	var ys []int
	ys = append(ys, c.Ymin)
	if c.Ymin < o.Ymin && o.Ymin < c.Ymax {
		ys = append(ys, o.Ymin)
	}
	if c.Ymin < o.Ymax && o.Ymax < c.Ymax {
		ys = append(ys, o.Ymax)
	}
	ys = append(ys, c.Ymax)

	var zs []int
	zs = append(zs, c.Zmin)
	if c.Zmin < o.Zmin && o.Zmin < c.Zmax {
		zs = append(zs, o.Zmin)
	}
	if c.Zmin < o.Zmax && o.Zmax < c.Zmax {
		zs = append(zs, o.Zmax)
	}
	zs = append(zs, c.Zmax)

	for i := 0; i < len(xs)-1; i++ {
		xfrom := xs[i]
		xto := xs[i+1]
		for j := 0; j < len(ys)-1; j++ {
			yfrom := ys[j]
			yto := ys[j+1]
			for k := 0; k < len(zs)-1; k++ {
				zfrom := zs[k]
				zto := zs[k+1]
				nc := Cuboid{Xmin: xfrom, Xmax: xto, Ymin: yfrom, Ymax: yto, Zmin: zfrom, Zmax: zto}
				if o.Contains(nc) || nc.Volume() <= 0 {
					continue
				}
				cs = append(cs, nc)
			}
		}
	}
	return cs
}

func (c Cuboid) GetOverlappingCuboid(o Cuboid) (Cuboid, error) {
	if !c.Overlaps(o) {
		return Cuboid{}, errors.New("Cubes do not overlap.")
	}

	oc := Cuboid{
		Xmin: int(math.Max(float64(c.Xmin), float64(o.Xmin))),
		Xmax: int(math.Min(float64(c.Xmax), float64(o.Xmax))),
		Ymin: int(math.Max(float64(c.Ymin), float64(o.Ymin))),
		Ymax: int(math.Min(float64(c.Ymax), float64(o.Ymax))),
		Zmin: int(math.Max(float64(c.Zmin), float64(o.Zmin))),
		Zmax: int(math.Min(float64(c.Zmax), float64(o.Zmax))),
	}

	return oc, nil
}

func (c Cuboid) Split(o Cuboid) (cs []Cuboid, err error) {
	oc, err := c.GetOverlappingCuboid(o)
	if err != nil {
		return []Cuboid{}, err
	}

	// Front face
	ftl := Cuboid{
		Xmin: c.Xmin,
		Xmax: oc.Xmin,
		Ymin: oc.Ymax,
		Ymax: c.Ymax,
		Zmin: c.Zmin,
		Zmax: oc.Zmin,
	}
	fml := Cuboid{
		Xmin: c.Xmin,
		Xmax: oc.Xmin,
		Ymin: oc.Ymin,
		Ymax: oc.Ymax,
		Zmin: c.Zmin,
		Zmax: oc.Zmin,
	}
	fbl := Cuboid{
		Xmin: c.Xmin,
		Xmax: oc.Xmin,
		Ymin: c.Ymin,
		Ymax: oc.Ymin,
		Zmin: c.Zmin,
		Zmax: oc.Zmin,
	}
	ftr := Cuboid{
		Xmin: oc.Xmin,
		Xmax: c.Xmax,
		Ymin: oc.Ymax,
		Ymax: c.Ymax,
		Zmin: c.Zmin,
		Zmax: oc.Zmin,
	}
	fmr := Cuboid{
		Xmin: oc.Xmin,
		Xmax: c.Xmax,
		Ymin: oc.Ymin,
		Ymax: oc.Ymax,
		Zmin: c.Zmin,
		Zmax: oc.Zmin,
	}
	fbr := Cuboid{
		Xmin: oc.Xmin,
		Xmax: c.Xmax,
		Ymin: c.Ymin,
		Ymax: oc.Ymin,
		Zmin: c.Zmin,
		Zmax: oc.Zmin,
	}
	fct := Cuboid{
		Xmin: oc.Xmin,
		Xmax: oc.Xmax,
		Ymin: oc.Ymax,
		Ymax: c.Ymax,
		Zmin: c.Zmin,
		Zmax: oc.Zmin,
	}
	fcc := Cuboid{
		Xmin: oc.Xmin,
		Xmax: oc.Xmax,
		Ymin: oc.Ymin,
		Ymax: oc.Ymax,
		Zmin: c.Zmin,
		Zmax: oc.Zmin,
	}
	fcb := Cuboid{
		Xmin: oc.Xmin,
		Xmax: oc.Xmax,
		Ymin: c.Ymin,
		Ymax: oc.Ymin,
		Zmin: c.Zmin,
		Zmax: oc.Zmin,
	}

	// Back face
	btl := Cuboid{
		Xmin: c.Xmin,
		Xmax: oc.Xmin,
		Ymin: oc.Ymax,
		Ymax: c.Ymax,
		Zmin: oc.Zmax,
		Zmax: c.Zmax,
	}
	bml := Cuboid{
		Xmin: c.Xmin,
		Xmax: oc.Xmin,
		Ymin: oc.Ymin,
		Ymax: oc.Ymax,
		Zmin: oc.Zmax,
		Zmax: c.Zmax,
	}
	bbl := Cuboid{
		Xmin: c.Xmin,
		Xmax: oc.Xmin,
		Ymin: c.Ymin,
		Ymax: oc.Ymin,
		Zmin: oc.Zmax,
		Zmax: c.Zmax,
	}
	btr := Cuboid{
		Xmin: oc.Xmin,
		Xmax: c.Xmax,
		Ymin: oc.Ymax,
		Ymax: c.Ymax,
		Zmin: oc.Zmax,
		Zmax: c.Zmax,
	}
	bmr := Cuboid{
		Xmin: oc.Xmin,
		Xmax: c.Xmax,
		Ymin: oc.Ymin,
		Ymax: oc.Ymax,
		Zmin: oc.Zmax,
		Zmax: c.Zmax,
	}
	bbr := Cuboid{
		Xmin: oc.Xmin,
		Xmax: c.Xmax,
		Ymin: c.Ymin,
		Ymax: oc.Ymin,
		Zmin: oc.Zmax,
		Zmax: c.Zmax,
	}

	bct := Cuboid{
		Xmin: oc.Xmin,
		Xmax: oc.Xmax,
		Ymin: oc.Ymax,
		Ymax: c.Ymax,
		Zmin: oc.Zmax,
		Zmax: c.Zmax,
	}
	bcc := Cuboid{
		Xmin: oc.Xmin,
		Xmax: oc.Xmax,
		Ymin: oc.Ymin,
		Ymax: oc.Ymax,
		Zmin: oc.Zmax,
		Zmax: c.Zmax,
	}
	bcb := Cuboid{
		Xmin: oc.Xmin,
		Xmax: oc.Xmax,
		Ymin: c.Ymin,
		Ymax: oc.Ymin,
		Zmin: oc.Zmax,
		Zmax: c.Zmax,
	}

	// Hidden face

	htl := Cuboid{
		Xmin: c.Xmin,
		Xmax: oc.Xmin,
		Ymin: oc.Ymax,
		Ymax: c.Ymax,
		Zmin: oc.Zmin,
		Zmax: oc.Zmax,
	}
	hml := Cuboid{
		Xmin: c.Xmin,
		Xmax: oc.Xmin,
		Ymin: oc.Ymin,
		Ymax: oc.Ymax,
		Zmin: oc.Zmin,
		Zmax: oc.Zmax,
	}
	hbl := Cuboid{
		Xmin: c.Xmin,
		Xmax: oc.Xmin,
		Ymin: c.Ymin,
		Ymax: oc.Ymin,
		Zmin: oc.Zmin,
		Zmax: oc.Zmax,
	}
	htr := Cuboid{
		Xmin: oc.Xmin,
		Xmax: c.Xmax,
		Ymin: oc.Ymax,
		Ymax: c.Ymax,
		Zmin: oc.Zmin,
		Zmax: oc.Zmax,
	}
	hmr := Cuboid{
		Xmin: oc.Xmin,
		Xmax: c.Xmax,
		Ymin: oc.Ymin,
		Ymax: oc.Ymax,
		Zmin: oc.Zmin,
		Zmax: oc.Zmax,
	}
	hbr := Cuboid{
		Xmin: oc.Xmin,
		Xmax: c.Xmax,
		Ymin: c.Ymin,
		Ymax: oc.Ymin,
		Zmin: oc.Zmin,
		Zmax: oc.Zmax,
	}

	hct := Cuboid{
		Xmin: oc.Xmin,
		Xmax: oc.Xmax,
		Ymin: oc.Ymax,
		Ymax: c.Ymax,
		Zmin: oc.Zmin,
		Zmax: oc.Zmax,
	}
	hcc := Cuboid{
		Xmin: oc.Xmin,
		Xmax: oc.Xmax,
		Ymin: oc.Ymin,
		Ymax: oc.Ymax,
		Zmin: oc.Zmin,
		Zmax: oc.Zmax,
	}
	hcb := Cuboid{
		Xmin: oc.Xmin,
		Xmax: oc.Xmax,
		Ymin: c.Ymin,
		Ymax: oc.Ymin,
		Zmin: oc.Zmin,
		Zmax: oc.Zmax,
	}
	uc := map[Cuboid]bool{
		ftl: true,
		fml: true,
		fbl: true,
		fct: true,
		fcc: true,
		fcb: true,
		ftr: true,
		fmr: true,
		fbr: true,
		btl: true,
		bml: true,
		bbl: true,
		bct: true,
		bcc: true,
		bcb: true,
		btr: true,
		bmr: true,
		bbr: true,

		htl: true,
		hml: true,
		hbl: true,
		hct: true,
		hcc: true,
		hcb: true,
		htr: true,
		hmr: true,
		hbr: true,
	}

	for c := range uc {
		if c.Volume() > 1 {
			fmt.Println(c, c.Volume())
			cs = append(cs, c)
		}
	}
	return cs, nil
}

func Part2(steps []Step) (co int) {
	onranges := make([]Cuboid, 0)
	for _, s := range steps {
		c := CuboidFromStep(s)

		ncs := make([]Cuboid, 0)
		for _, o := range onranges {
			ncs = append(ncs, o.Subtract(c)...)
		}
		onranges = make([]Cuboid, len(ncs))
		copy(onranges, ncs)

		if s.On {
			onranges = append(onranges, c)
		}
	}
	for _, c := range onranges {
		co += c.Volume()
	}
	return
}

func main() {
	steps := ReadInstructions("input.txt")
	fmt.Println("Part 1 result:", Part1(steps))
	fmt.Println("Part 2 result:", Part2(steps))
}
