package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"math"
)

type Binary []bool

func (b Binary) Int() (v int) {
	l := len(b)
	for i, bin := range b {
		if bin {
			v += int(math.Pow(2, float64(l-i-1)))
		}
	}
	return
}

func (b Binary) Print() {
	for _, bin := range b {
		if bin {
			fmt.Print("1")
		} else {
			fmt.Print("0")
		}
	}
	fmt.Printf(" (%d)\n", b.Int())
}

func BinaryMapping(c byte) []bool {
	return map[byte][]bool{
		'0': {false, false, false, false},
		'1': {false, false, false, true},
		'2': {false, false, true, false},
		'3': {false, false, true, true},
		'4': {false, true, false, false},
		'5': {false, true, false, true},
		'6': {false, true, true, false},
		'7': {false, true, true, true},
		'8': {true, false, false, false},
		'9': {true, false, false, true},
		'A': {true, false, true, false},
		'B': {true, false, true, true},
		'C': {true, true, false, false},
		'D': {true, true, false, true},
		'E': {true, true, true, false},
		'F': {true, true, true, true},
	}[c]
}

func ReadTransmission(fpath string) Binary {
	buf, err := ioutil.ReadFile(fpath)
	if err != nil {
		log.Fatal(err)
	}
	bin := make(Binary, len(buf)*4)
	for i, b := range buf {
		start := i * 4
		end := start + 4
		copy(bin[start:end], BinaryMapping(b))
	}
	return bin
}

type Header struct {
	Version int
	TypeId  int
}

func HeaderFromBits(b Binary) Header {
	if len(b) != 6 {
		log.Fatal("Header must have 6 bits.")
	}

	return Header{
		Version: b[:3].Int(),
		TypeId:  b[3:6].Int(),
	}
}

type Packet struct {
	Header     Header
	Literal    int
	SubPackets []Packet
}

func ParsePacket(b *Binary) Packet {
	h := HeaderFromBits((*b)[:6])
	(*b) = (*b)[6:]
	if h.TypeId == 4 {
		literal := ParseLiteralValue(b)
		return Packet{Header: h, Literal: literal}
	} else {
		sps := make([]Packet, 0)
		I := (*b)[0]
		(*b) = (*b)[1:]
		if I {
			n := (*b)[:11].Int()
			(*b) = (*b)[11:]
			for i := 0; i < n; i++ {
				sps = append(sps, ParsePacket(b))
			}
		} else {
			n := (*b)[:15].Int()
			(*b) = (*b)[15:]
			pbits := (*b)[:n]
			for len(pbits) > 8 {
				sps = append(sps, ParsePacket(&pbits))
			}
			(*b) = (*b)[n:]
		}
		return Packet{Header: h, SubPackets: sps}
	}
}

func ParseLiteralValue(b *Binary) int {
	literal := make(Binary, 0)
	if !(*b)[0] {
		literal = append(literal, (*b)[1:5]...)
		(*b) = (*b)[5:]
	} else {
		for (*b)[0] {
			literal = append(literal, (*b)[1:5]...)
			(*b) = (*b)[5:]
		}
		literal = append(literal, (*b)[1:5]...)
		(*b) = (*b)[5:]
	}
	return literal.Int()
}

func SumVersions(ps []Packet) (versions int) {
	for _, p := range ps {
		versions += p.Header.Version
		if p.Header.TypeId != 4 {
			versions += SumVersions(p.SubPackets)
		}
	}
	return
}

func SolveOperator(p Packet) Packet {
	ls := make([]Packet, 0)
	for _, sp := range p.SubPackets {
		if sp.Header.TypeId == 4 {
			ls = append(ls, sp)
		} else {
			ls = append(ls, SolveOperator(sp))
		}
	}

	if p.Header.TypeId == 0 { // Sum
		var sum int
		for _, l := range ls {
			sum += l.Literal
		}
		return Packet{Header: Header{TypeId: 4}, Literal: sum}
	} else if p.Header.TypeId == 1 { // Product
		prod := 1
		for _, l := range ls {
			prod *= l.Literal
		}
		return Packet{Header: Header{TypeId: 4}, Literal: prod}
	} else if p.Header.TypeId == 2 { // Min
		min := math.MaxInt
		var minP Packet
		for _, l := range ls {
			if l.Literal < min {
				min = l.Literal
				minP = l
			}
		}
		return minP
	} else if p.Header.TypeId == 3 { // Max
		max := math.MinInt
		var maxP Packet
		for _, l := range ls {
			if l.Literal > max {
				max = l.Literal
				maxP = l
			}
		}
		return maxP
	} else if p.Header.TypeId == 5 { // GT
		if len(ls) != 2 {
			log.Fatal("Invalid GT")
		}
		if ls[0].Literal > ls[1].Literal {
			return Packet{Header: Header{TypeId: 4}, Literal: 1}
		} else {
			return Packet{Header: Header{TypeId: 4}, Literal: 0}
		}
	} else if p.Header.TypeId == 6 { // LT
		if len(ls) != 2 {
			log.Fatal("Invalid LT")
		}
		if ls[0].Literal < ls[1].Literal {
			return Packet{Header: Header{TypeId: 4}, Literal: 1}
		} else {
			return Packet{Header: Header{TypeId: 4}, Literal: 0}
		}
	} else { // EQ
		if len(ls) != 2 {
			log.Fatal("Invalid EQ")
		}
		if ls[0].Literal == ls[1].Literal {
			return Packet{Header: Header{TypeId: 4}, Literal: 1}
		} else {
			return Packet{Header: Header{TypeId: 4}, Literal: 0}
		}
	}
}

func ParseHierarchy(tx Binary) []Packet {
	ps := make([]Packet, 0)
	lbs := tx
	for len(lbs) > 8 {
		ps = append(ps, ParsePacket(&lbs))
	}
	return ps
}

func main() {
	tx := ReadTransmission("input.txt")
	hr := ParseHierarchy(tx)
	fmt.Println("Part 1 result:", SumVersions(hr))
	fmt.Println("Part 2 result:", SolveOperator(hr[0]))
}
