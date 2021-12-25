package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

type Cucumber rune

type Position struct {
	i int
	j int
}

type Cucumbers map[Position]bool

type SeaDepth struct {
	SouthCucumbers Cucumbers
	EastCucumbers  Cucumbers
	maxi           int
	maxj           int
	WontMove       bool
}

func (sd SeaDepth) Step() SeaDepth {
	nsd := SeaDepth{
		SouthCucumbers: make(Cucumbers),
		EastCucumbers:  make(Cucumbers),
		maxi:           sd.maxi,
		maxj:           sd.maxj,
		WontMove:       false,
	}

	hasmoved := false
	for pos := range sd.EastCucumbers {
		r := Position{i: pos.i, j: (pos.j + 1) % nsd.maxj}
		_, ef := sd.EastCucumbers[r]
		_, sf := sd.SouthCucumbers[r]

		if !ef && !sf {
			hasmoved = true
			nsd.EastCucumbers[r] = true
		} else {
			nsd.EastCucumbers[pos] = true
		}
	}

	edsd := SeaDepth{
		SouthCucumbers: sd.SouthCucumbers,
		EastCucumbers:  nsd.EastCucumbers,
		maxi:           sd.maxi,
		maxj:           sd.maxj,
		WontMove:       false,
	}

	for pos := range sd.SouthCucumbers {
		d := Position{i: (pos.i + 1) % sd.maxi, j: pos.j}
		_, ef := edsd.EastCucumbers[d]
		_, sf := edsd.SouthCucumbers[d]

		if !ef && !sf {
			hasmoved = true
			nsd.SouthCucumbers[d] = true
		} else {
			nsd.SouthCucumbers[pos] = true
		}
	}

	if !hasmoved {
		nsd.WontMove = true
	}

	return nsd
}

func ReadSeaDepths(fpath string) SeaDepth {
	f, err := os.Open(fpath)
	if err != nil {
		log.Fatal(err)
	}
	scanner := bufio.NewScanner(f)
	sd := SeaDepth{
		SouthCucumbers: make(Cucumbers),
		EastCucumbers:  make(Cucumbers),
	}
	var i int
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		sd.maxj = len(line)
		for j, c := range strings.TrimSpace(scanner.Text()) {
			pos := Position{i: i, j: j}
			if c == '>' {
				sd.EastCucumbers[pos] = true
			} else if c == 'v' {
				sd.SouthCucumbers[pos] = true
			}
		}
		i++
	}
	sd.maxi = i
	return sd
}

func (sd SeaDepth) Print() {
	for i := 0; i < sd.maxi; i++ {
		for j := 0; j < sd.maxj; j++ {
			p := Position{i: i, j: j}
			_, ef := sd.EastCucumbers[p]
			_, sf := sd.SouthCucumbers[p]
			if ef {
				fmt.Print(">")
			} else if sf {
				fmt.Print("v")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
}

func main() {
	sd := ReadSeaDepths("input.txt")
	var i int
	for {
		sd = sd.Step()
		if sd.WontMove {
			sd.Print()
			fmt.Println("Won't move further step: ", i+1)
			break
		}
		i++
	}
}
