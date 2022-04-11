package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"strings"
)

type Point struct {
	i int
	j int
}

type Image struct {
	Pixels     map[Point]int
	Background int
}

func ImageFromText(text string) Image {
	rows := strings.Split(text, "\n")
	im := make(map[Point]int)
	for i, row := range rows {
		for j, col := range strings.TrimSpace(row) {
			if col == '.' {
				im[Point{i: i, j: j}] = 0
			} else {
				im[Point{i: i, j: j}] = 1
			}
		}
	}
	return Image{Pixels: im, Background: 0}
}

func (im Image) Sum() (sum int) {
	for _, i := range im.Pixels {
		sum += i
	}
	return
}

func (im Image) PatchValue(ci, cj int) int {
	v := 0
	pos := 8
	for i := ci - 1; i <= ci+1; i++ {
		for j := cj - 1; j <= cj+1; j++ {
			p := Point{i: i, j: j}
			pv, pixelpresent := im.Pixels[p]
			if pixelpresent {
				if pv == 1 {
					v += int(math.Pow(2, float64(pos)))
				}
			} else if im.Background == 1 {
				v += int(math.Pow(2, float64(pos)))
			}
			pos--
		}
	}

	return v
}

type ImageEnhancerAlgorithm string

type ImageEnhancer struct {
	Algorithm  ImageEnhancerAlgorithm
	InputImage Image
}

func (ie ImageEnhancer) Enhance() ImageEnhancer {
	mini := math.MaxInt
	minj := math.MaxInt
	maxi := math.MinInt
	maxj := math.MinInt
	for p := range ie.InputImage.Pixels {
		if p.i < mini {
			mini = p.i
		}
		if p.j < minj {
			minj = p.j
		}
		if p.i > maxi {
			maxi = p.i
		}
		if p.j > maxj {
			maxj = p.j
		}
	}

	oim := make(map[Point]int)
	offset := 1
	for i := mini - offset; i <= maxi+offset; i++ {
		for j := minj - offset; j <= maxj+offset; j++ {
			indexer := ie.InputImage.PatchValue(i, j)
			if ie.Algorithm[indexer] == '.' {
				oim[Point{i: i, j: j}] = 0
			} else {
				oim[Point{i: i, j: j}] = 1
			}
		}
	}
	var nb int
	if ie.InputImage.Background == 1 {
		if ie.Algorithm[len(ie.Algorithm)-1] == '#' {
			nb = 1
		} else {
			nb = 0
		}
	} else {
		if ie.Algorithm[0] == '#' {
			nb = 1
		} else {
			nb = 0
		}
	}
	return ImageEnhancer{Algorithm: ie.Algorithm, InputImage: Image{Pixels: oim, Background: nb}}
}

func ReadInput(fpath string) ImageEnhancer {
	buf, err := ioutil.ReadFile(fpath)
	if err != nil {
		log.Fatal(err)
	}
	fcontent := string(buf)
	sections := strings.Split(fcontent, "\n\n")
	return ImageEnhancer{
		Algorithm:  ImageEnhancerAlgorithm(strings.TrimSpace(sections[0])),
		InputImage: ImageFromText(sections[1]),
	}
}

func (ie ImageEnhancer) Part1() int {
	nie := ie.Enhance()
	nie = nie.Enhance()
	return nie.InputImage.Sum()
}

func (ie ImageEnhancer) Part2() int {
	nie := ie
	for i := 0; i < 50; i++ {
		nie = nie.Enhance()
	}
	return nie.InputImage.Sum()
}
func main() {
	ie := ReadInput("input.txt")
	fmt.Println("Part 1 result:", ie.Part1())
	fmt.Println("Part 2 result:", ie.Part2())
}
