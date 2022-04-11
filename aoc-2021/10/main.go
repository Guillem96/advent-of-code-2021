package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"sort"
	"strings"
)

type Token rune

func (t Token) IsOpening() bool {
	return t == '{' || t == '[' || t == '(' || t == '<'
}

func (t Token) IsClosing() bool {
	return t == '}' || t == ']' || t == ')' || t == '>'
}

func (t Token) CloseRespective() Token {
	return map[rune]Token{
		'(': Token(')'),
		'{': Token('}'),
		'[': Token(']'),
		'<': Token('>'),
	}[rune(t)]
}

func (t Token) OpenRespective() Token {
	return map[rune]Token{
		')': Token('('),
		'}': Token('{'),
		']': Token('['),
		'>': Token('<'),
	}[rune(t)]
}

func (t Token) Points() int {
	return map[rune]int{
		')': 3,
		'}': 1197,
		']': 57,
		'>': 25137,
		'(': 1,
		'[': 2,
		'{': 3,
		'<': 4,
	}[rune(t)]
}

type Stack []Token

func (s *Stack) Push(e Token) {
	*s = append(*s, e)
}

func (s *Stack) IsEmpty() bool {
	return len(*s) == 0
}

func (s *Stack) Peek() Token {
	return (*s)[len(*s)-1]
}

func (s *Stack) Pop() Token {
	p := s.Peek()
	*s = (*s)[:len(*s)-1]
	return p
}

func ReadTokens(s string) (res []Token) {
	for _, c := range strings.TrimSpace(s) {
		res = append(res, Token(c))
	}
	return res
}

func Part1(scanner *bufio.Scanner) (res int) {
	for scanner.Scan() {
		ts := ReadTokens(scanner.Text())
		stack := make(Stack, 0)
		for _, t := range ts {
			if t.IsOpening() {
				stack.Push(t)
			} else if stack.IsEmpty() || stack.Pop().CloseRespective() != t {
				res += t.Points()
				break
			}
		}
	}
	return res
}

func SortedInsert(s []int, n int) []int {
	if len(s) == 0 {
		s = append(s, n)
		return s
	}
	i := sort.Search(len(s), func(i int) bool { return s[i] > n })
	s = append(s, -1)
	copy(s[i+1:], s[i:])
	s[i] = n
	return s
}

func Part2(scanner *bufio.Scanner) (res int) {
	var ls []int
	for scanner.Scan() {
		ts := ReadTokens(scanner.Text())
		wrong := false
		stack := make(Stack, 0)
		for _, t := range ts {
			if t.IsOpening() {
				stack.Push(t)
			} else if stack.IsEmpty() || stack.Pop().CloseRespective() != t {
				wrong = true
				break
			}
		}
		if !wrong {
			score := 0
			for !stack.IsEmpty() {
				score = score*5 + stack.Pop().Points()
			}
			ls = SortedInsert(ls, score)
		}
	}
	return ls[int(math.Floor(float64(len(ls))/2.0))]
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	scanner := bufio.NewScanner(f)
	// fmt.Println("Result part 1:", Part1(scanner))
	fmt.Println("Result part 2:", Part2(scanner))
}
