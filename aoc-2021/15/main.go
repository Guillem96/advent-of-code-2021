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

type Point struct {
	i    int
	j    int
	cost int
}

type PriorityItem struct {
	Point    Point
	Priority int
}

type PriorityQueue map[Point]int

func (pq PriorityQueue) Add(v Point, priority int) {
	_, found := pq[v]
	if found {
		log.Fatalf("Duplicated register %+v...", v)
	}
	pq[v] = priority
}

func (pq PriorityQueue) GetPriority(p Point) int {
	_, found := pq[p]
	if !found {
		log.Fatal("GetPriority on a non existent point")
	}
	return pq[p]
}

func (pq PriorityQueue) Deque() PriorityItem {
	minP := math.MaxInt
	p := Point{i: -1, j: -1}
	for point, prio := range pq {
		if prio < minP {
			p = point
			minP = prio
		}
	}
	pi := PriorityItem{Point: p, Priority: minP}
	delete(pq, p)
	return pi
}

func (pq PriorityQueue) UpdatePriority(p Point, priority int) {
	_, found := pq[p]
	if !found {
		log.Fatal("Update priority on a non existent point")
	}
	pq[p] = priority
}

type Grid [][]Point

func (g Grid) GetAdj(p Point) []Point {
	var adj []Point
	if p.i-1 > 0 {
		adj = append(adj, g[p.i-1][p.j])
	}

	if p.i+1 < len(g) {
		adj = append(adj, g[p.i+1][p.j])
	}

	if p.j-1 > 0 {
		adj = append(adj, g[p.i][p.j-1])
	}

	if p.j+1 < len(g[0]) {
		adj = append(adj, g[p.i][p.j+1])
	}
	return adj
}

func (g Grid) FindBestPath() int {
	// Initialize queue
	pq := make(PriorityQueue)
	for i := 0; i < len(g); i++ {
		for j := 0; j < len(g[i]); j++ {
			p := g[i][j]
			if p.i == 0 && p.j == 0 {
				pq.Add(p, 0)
			} else {
				pq.Add(p, math.MaxInt)
			}
		}
	}

	visited := make(map[Point]bool)
	weights := make(map[Point]int)
	weights[g[0][0]] = 0

	for len(pq) > 0 {
		c := pq.Deque()
		visited[c.Point] = true
		weights[c.Point] = c.Priority
		if c.Point.i == len(g)-1 && c.Point.j == len(g[0])-1 {
			break
		}
		for _, adj := range g.GetAdj(c.Point) {
			_, found := visited[adj]
			if found {
				continue
			}
			edgeW := weights[c.Point] + adj.cost
			adjPriority := pq.GetPriority(adj)

			if edgeW < adjPriority {
				pq.UpdatePriority(adj, edgeW)
			}
		}
	}
	return weights[g[len(g)-1][len(g[0])-1]]
}

func ReadGrid(fpath string) Grid {
	g := make(Grid, 0)

	f, err := os.Open(fpath)
	if err != nil {
		log.Fatal(err)
	}

	i := 0
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		l := strings.TrimSpace(scanner.Text())
		row := make([]Point, len(l))
		for j, c := range l {
			n, err := strconv.Atoi(string(c))
			if err != nil {
				log.Fatal(err)
			}
			row[j] = Point{i: i, j: j, cost: n}
		}
		i++
		g = append(g, row)
	}
	return g
}

func (g Grid) ExpandGrid(times int) Grid {
	ng := make(Grid, len(g)*times)
	for i := 0; i < len(g)*times; i++ {
		ng[i] = make([]Point, len(g[0])*times)
		for j := 0; j < len(g[0])*times; j++ {
			gi := i % len(g)
			gj := j % len(g[0])
			p := g[gi][gj]
			sum := int(i/len(g)) + int(j/len(g[0]))
			nc := p.cost + sum
			if nc > 9 {
				nc = nc - 9
			}
			ng[i][j] = Point{i: i, j: j, cost: nc}
		}
	}
	return ng
}

func main() {
	g := ReadGrid("input.txt")
	fmt.Println("Part 1 result:", g.FindBestPath())
	fmt.Println("Part 2 result:", g.ExpandGrid(5).FindBestPath())
}
