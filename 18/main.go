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

type Number struct {
	Value  int
	Left   *Number
	Right  *Number
	Parent *Number
}

func (n *Number) Print() {
	if n.IsNatural() {
		fmt.Print(n.Value)
	} else {
		fmt.Print("[")
		n.Left.Print()
		fmt.Print(",")
		n.Right.Print()
		fmt.Print("]")
	}
}

func (n *Number) IsNatural() bool {
	return n.Right == nil && n.Left == nil
}

func (n *Number) Add(other *Number) *Number {
	nn := &Number{Left: n, Right: other, Parent: nil}
	n.Parent = nn
	other.Parent = nn
	return nn
}

func (n *Number) Split() bool {
	if n.IsNatural() {
		if n.Value >= 10 {
			fp := int(math.Floor(float64(n.Value) / 2))
			sp := int(math.Ceil(float64(n.Value) / 2))
			p := n.Parent
			if n == p.Left {
				p.Left.Value = 0
				p.Left.Left = &Number{Value: fp, Parent: p.Left}
				p.Left.Right = &Number{Value: sp, Parent: p.Left}
			} else {
				p.Right.Value = 0
				p.Right.Left = &Number{Value: fp, Parent: p.Right}
				p.Right.Right = &Number{Value: sp, Parent: p.Right}
			}
			return true
		}
		return false
	}
	return n.Left.Split() || n.Right.Split()
}

func (n *Number) Reduce() {
	he := n.Explode(0)
	if he {
		n.Reduce()
	} else {
		hs := n.Split()
		if hs {
			n.Reduce()
		}
	}
}

func (n *Number) Explode(level int) bool {
	if n.IsNatural() {
		return false
	}

	if level == 4 {
		v := make(map[*Number]bool)
		v[n] = true
		ln := n.Parent.FindClosestLeft(v)
		if ln != nil {
			tmp := n.Left.Value
			tmpV := ln.Value
			ln.Value = tmp + tmpV
		}

		v = make(map[*Number]bool)
		v[n] = true
		rn := n.Parent.FindClosestRight(v)
		if rn != nil {
			tmp := n.Right.Value
			tmpV := rn.Value
			rn.Value = tmp + tmpV
		}

		p := n.Parent
		if n == p.Left {
			p.Left.Right = nil
			p.Left.Left = nil
			p.Left.Value = 0
		} else {
			p.Right.Value = 0
			p.Right.Left = nil
			p.Right.Right = nil
		}
		return true
	} else {
		return n.Left.Explode(level+1) || n.Right.Explode(level+1)
	}
}

func (n *Number) Magnitude() int {
	if n.IsNatural() {
		return n.Value
	} else {
		return 3*n.Left.Magnitude() + 2*n.Right.Magnitude()
	}
}

func (n *Number) FindClosestLeft(visited map[*Number]bool) *Number {
	if n == nil {
		return nil
	}

	visited[n] = true
	_, found := visited[n.Left]
	if found {
		return n.Parent.FindClosestLeft(visited)
	}

	if n.Left.IsNatural() {
		return n.Left
	} else {
		it := n.Left
		for !it.IsNatural() {
			it = it.Right
		}
		return it
	}
}

func (n *Number) FindClosestRight(visited map[*Number]bool) *Number {
	if n == nil {
		return nil
	}

	visited[n] = true
	_, found := visited[n.Right]
	if found {
		return n.Parent.FindClosestRight(visited)
	}

	if n.Right.IsNatural() {
		return n.Right
	} else {
		it := n.Right
		for !it.IsNatural() {
			it = it.Left
		}
		return it
	}
}

func NumberFromText(text string) *Number {
	text = strings.TrimSpace(text)
	stack := make([]*Number, 0)
	pof := &Number{Parent: nil}
	cn := pof
	fr := false
	for _, b := range text[1:] {
		if b == '[' {
			nn := &Number{Parent: cn}
			if fr {
				cn.Right = nn
				fr = false
			} else {
				cn.Left = nn
			}
			stack = append(stack, cn)
			cn = nn
		} else if b == ']' {
			if len(stack) == 0 {
				break
			}
			cn = stack[len(stack)-1]
			stack = stack[:len(stack)-1]
		} else if b == ',' {
			fr = true
		} else {
			pn, err := strconv.Atoi(string(b))
			if err != nil {
				log.Fatal(err)
			}
			if fr {
				cn.Right = &Number{Value: pn, Parent: cn}
				fr = false
			} else {
				cn.Left = &Number{Value: pn, Parent: cn}
			}
		}
	}
	return pof
}

func ReadOperands(fpath string) []*Number {
	f, err := os.Open(fpath)
	if err != nil {
		log.Fatal(err)
	}
	ns := make([]*Number, 0)
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		ns = append(ns, NumberFromText(scanner.Text()))
	}
	return ns
}

func AddNumbers(ns []*Number) int {
	res := ns[0].Copy(nil)
	for i := 1; i < len(ns); i++ {
		res = res.Add(ns[i].Copy(nil))
		res.Reduce()
	}
	return res.Magnitude()
}

func (n *Number) Copy(parent *Number) *Number {
	if n.IsNatural() {
		return &Number{Value: n.Value, Parent: parent}
	} else {
		np := &Number{Parent: parent}
		np.Right = n.Right.Copy(np)
		np.Left = n.Left.Copy(np)
		return np
	}
}

func LargestMagnitude(ns []*Number) int {
	mm := math.MinInt64
	for i := 0; i < len(ns); i++ {
		for j := 0; j < len(ns); j++ {
			if i != j {
				m := AddNumbers([]*Number{ns[i], ns[j]})
				if m > mm {
					mm = m
				}
			}
		}
	}
	return mm
}

func main() {
	ns := ReadOperands("input.txt")
	fmt.Println("Part 1 result:", AddNumbers(ns))
	fmt.Println("Part 2 result:", LargestMagnitude(ns))
}
