package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"strconv"
	"strings"
)

type TargetArea struct {
	Xmin int
	Xmax int
	Ymin int
	Ymax int
}

func (ta TargetArea) IsIn(x, y int) bool {
	return x >= ta.Xmin && x <= ta.Xmax && y >= ta.Ymin && y <= ta.Ymax
}

func (ta TargetArea) IsUnreachable(x, y, dx, dy int) bool {
	if x > ta.Xmax && dx >= 0 {
		return true
	}
	if x < ta.Xmin && dx <= 0 {
		return true
	}

	if y < ta.Ymin && dy < 0 {
		return true
	}

	return false
}

func ReadTargetArea(fpath string) TargetArea {
	buf, err := ioutil.ReadFile(fpath)
	if err != nil {
		log.Fatal(err)
	}
	tas := string(buf)
	tas = tas[len("target area: x="):]
	ranges := strings.Split(tas, ",")

	// Parse xs
	xrange := strings.Split(ranges[0], "..")
	xmin, err := strconv.Atoi(xrange[0])
	if err != nil {
		log.Fatal(err)
	}
	xmax, err := strconv.Atoi(xrange[1])
	if err != nil {
		log.Fatal(err)
	}

	// Parse ys
	yrange := strings.Split(ranges[1][3:], "..")
	ymin, err := strconv.Atoi(yrange[0])
	if err != nil {
		log.Fatal(err)
	}
	ymax, err := strconv.Atoi(yrange[1])
	if err != nil {
		log.Fatal(err)
	}
	return TargetArea{Xmin: xmin, Xmax: xmax, Ymin: ymin, Ymax: ymax}
}

func SimulateTrajectory(dx, dy int, ta TargetArea) (int, error) {
	var x int
	var y int
	dxDelta := -1
	if dx < 0 {
		dxDelta = 1
	}
	maxY := math.MinInt
	for {
		x += dx
		y += dy
		if y > maxY {
			maxY = y
		}

		if ta.IsIn(x, y) {
			return maxY, nil
		}

		if ta.IsUnreachable(x, y, dx, dy) {
			return 0, errors.New("Unrachable")
		}

		if dx != 0 {
			dx += dxDelta
		}
		dy -= 1
	}
}

func Part1(ta TargetArea) int {
	maxY := math.MinInt
	for x := 0; x < ta.Xmax; x++ {
		for y := -200; y < 200; y++ {
			height, err := SimulateTrajectory(x, y, ta)
			if err == nil && maxY < height {
				maxY = height
			}
		}
	}
	return maxY
}

func Part2(ta TargetArea) (count int) {
	for x := 0; x < ta.Xmax; x++ {
		for y := -200; y < 200; y++ {
			_, err := SimulateTrajectory(x, y, ta)
			if err == nil {
				count++
			}
		}
	}
	return
}

func main() {
	ta := ReadTargetArea("input.txt")
	fmt.Println("Part 1 result:", Part1(ta))
	fmt.Println("Part 2 result:", Part2(ta))
}
