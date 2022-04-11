package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

type Player struct {
	Position int
	Score    int
}

func PlayerFromText(txt string) Player {
	n := strings.TrimSpace(txt[len(txt)-2:])
	pos, err := strconv.Atoi(n)
	if err != nil {
		log.Fatal(err)
	}
	return Player{Score: 0, Position: pos}
}

func ReadPlayers(fpath string) []Player {
	buf, err := ioutil.ReadFile(fpath)
	if err != nil {
		log.Fatal(err)
	}
	fcontent := string(buf)
	players := strings.Split(fcontent, "\n")
	return []Player{
		PlayerFromText(players[0]),
		PlayerFromText(players[1]),
	}
}

func (p Player) MoveForward(move int) Player {
	if move > 10 {
		tt := int(move / 10)
		move = move - tt*10
	}

	np := p.Position + move
	if np > 10 {
		np = np - 10
	}

	return Player{Position: np, Score: p.Score + np}
}

func (p Player) HasWon() bool {
	return p.Score >= 1000
}

func (p Player) HasWon2() bool {
	return p.Score >= 21
}

type DeterministicDie struct {
	Rolled int
	Offset int
}

func (d *DeterministicDie) Roll() (forward int) {
	for i := d.Offset; i < d.Offset+3; i++ {
		d.Rolled++
		rv := i
		if rv > 100 {
			rv = rv - 100
		}
		forward += rv
	}
	if d.Offset+3 > 100 {
		d.Offset = d.Offset + 3 - 100
	} else {
		d.Offset += 3
	}
	return
}

func Part1(players []Player) int {
	die := &DeterministicDie{Rolled: 0, Offset: 1}
	p1 := players[0]
	p2 := players[1]

	for {
		p1 = p1.MoveForward(die.Roll())
		if p1.HasWon() {
			fmt.Println("Player 1 wins!!!")
			fmt.Println("Die rolled", die.Rolled, "times")
			fmt.Println("Losing player scored", p2.Score, "points")
			return p2.Score * die.Rolled
		}

		p2 = p2.MoveForward(die.Roll())
		if p2.HasWon() {
			fmt.Println("Player 1 wins!!!")
			fmt.Println("Die rolled", die.Rolled, "times")
			fmt.Println("Losing player scored", p1.Score, "points")
			return p1.Score * die.Rolled
		}
	}
}

type CacheIndex struct {
	Player1 Player
	Player2 Player
	TurnOf  int
	Rc      int
	am      int
}

var cache map[CacheIndex][]int

func Turn(ps []Player, turnOf int, rc int, am int) []int {
	ci := CacheIndex{
		Player1: ps[0],
		Player2: ps[1],
		TurnOf:  turnOf,
		Rc:      rc,
		am:      am,
	}
	v, found := cache[ci]
	if found {
		return v
	}

	wins := []int{0, 0}
	if rc == 3 {
		np := ps[turnOf].MoveForward(am)
		if np.HasWon2() {
			wins[turnOf] = 1
			return wins
		} else {
			nps := make([]Player, 2)
			copy(nps, ps)
			nps[turnOf] = np
			return SumArrays(wins, Turn(nps, 1-turnOf, 0, 0))
		}
	} else {
		for i := 1; i <= 3; i++ {
			wins = SumArrays(wins, Turn(ps, turnOf, rc+1, am+i))
			cache[ci] = wins
		}
	}

	return wins
}

func SumArrays(a, b []int) []int {
	res := make([]int, len(a))
	for i, v := range a {
		res[i] = v + b[i]
	}
	return res
}

func Part2(players []Player) []int {
	cache = make(map[CacheIndex][]int)
	return Turn(players, 0, 0, 0)
}

func main() {
	players := ReadPlayers("input.txt")
	fmt.Println("Part 1 result:", Part1(players))
	fmt.Println("Part 2 result:", Part2(players))
}
