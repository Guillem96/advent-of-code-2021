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

func ReadIntLine(s string, sep string) []int {
	var ls []string
	if sep == " " {
		ls = strings.Fields(s)
	} else {
		ls = strings.Split(strings.TrimSpace(s), sep)
	}
	var res []int
	for _, ns := range ls {
		in, err := strconv.Atoi(ns)
		if err != nil {
			log.Fatal(err)
		}
		res = append(res, in)
	}
	return res
}

type Number struct {
	row   int
	col   int
	value int
	said  bool
}

type Board struct {
	numbers  []*Number
	toWinRow []int
	toWinCol []int
}

func BoardFromNumbers(ns []string) *Board {
	b := &Board{
		numbers:  make([]*Number, 0),
		toWinRow: []int{5, 5, 5, 5, 5},
		toWinCol: []int{5, 5, 5, 5, 5},
	}
	for row, rns := range ns {
		cns := ReadIntLine(rns, " ")
		for col, cn := range cns {
			b.AddNumber(&Number{
				col:   col,
				row:   row,
				value: cn,
				said:  false,
			})
		}
	}
	return b
}

func (b *Board) AddNumber(n *Number) {
	if len(b.numbers) == 0 {
		b.numbers = append(b.numbers, n)
		return
	}
	i := sort.Search(len(b.numbers), func(i int) bool { return b.numbers[i].value > n.value })
	b.numbers = append(b.numbers, &Number{row: -1, col: -1, value: -1, said: false})
	copy(b.numbers[i+1:], b.numbers[i:])
	b.numbers[i] = n
}

func (b *Board) MarkNumber(n int) bool {
	i := sort.Search(len(b.numbers), func(i int) bool { return b.numbers[i].value >= n })
	if i < len(b.numbers) && b.numbers[i].value == n {
		bn := b.numbers[i]
		bn.said = true
		b.toWinCol[bn.col]--
		b.toWinRow[bn.row]--
		return b.toWinCol[bn.col] == 0 || b.toWinRow[bn.row] == 0
	}
	return false
}

func (b *Board) Score(lastNumber int) int {
	sum := 0
	for _, bn := range b.numbers {
		if !bn.said {
			sum += bn.value
		}
	}
	return sum * lastNumber
}

type Game struct {
	boards    []*Board
	randomSeq []int
}

func (g *Game) StartPart1() int {
	for _, rn := range g.randomSeq {
		for _, b := range g.boards {
			if b.MarkNumber(rn) {
				return b.Score(rn)
			}
		}
	}
	return -1
}

func (g *Game) StartPart2() int {
	points := make([]int, len(g.boards))
	lw := -1
	for i := 0; i < len(points); i++ {
		points[i] = -1
	}

	for _, rn := range g.randomSeq {
		for j, b := range g.boards {
			if points[j] == -1 && b.MarkNumber(rn) {
				lw = j
				fmt.Println(lw, rn)
				points[j] = b.Score(rn)
			}
		}
	}
	return points[lw]
}

func GameFromFile(scanner *bufio.Scanner) *Game {
	scanner.Scan()
	seqNumbers := ReadIntLine(scanner.Text(), ",")
	boards := make([]*Board, 0)
	scanner.Scan()

	ns := make([]string, 5)
	i := 0
	for scanner.Scan() {
		l := scanner.Text()
		if l == "" {
			boards = append(boards, BoardFromNumbers(ns))
			i = 0
		} else {
			ns[i] = l
			i++
		}
	}
	boards = append(boards, BoardFromNumbers(ns))

	return &Game{
		boards:    boards,
		randomSeq: seqNumbers,
	}
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	g := GameFromFile(scanner)
	fmt.Println("# Boards:", len(g.boards))

	// fmt.Println("Part 1 result:", g.StartPart1())
	fmt.Println("Part 2 result:", g.StartPart2())
}
