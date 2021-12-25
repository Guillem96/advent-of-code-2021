package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"sort"
)

const IsPart1 = false
const NA = 16

const (
	Ambar  = 'A'
	Bronze = 'B'
	Copper = 'C'
	Desert = 'D'
)

type Position struct {
	i int
	j int
}

var (
	AmbarHomeTop          = Position{i: 2, j: 3}
	AmbarHomeCenterTop    = Position{i: 3, j: 3}
	AmbarHomeCenterBottom = Position{i: 4, j: 3}
	AmbarHomeBottom       = Position{i: 5, j: 3}

	BronzeHomeTop          = Position{i: 2, j: 5}
	BronzeHomeCenterTop    = Position{i: 3, j: 5}
	BronzeHomeCenterBottom = Position{i: 4, j: 5}
	BronzeHomeBottom       = Position{i: 5, j: 5}

	CopperHomeTop          = Position{i: 2, j: 7}
	CopperHomeCenterTop    = Position{i: 3, j: 7}
	CopperHomeCenterBottom = Position{i: 4, j: 7}
	CopperHomeBottom       = Position{i: 5, j: 7}

	DesertHomeTop          = Position{i: 2, j: 9}
	DesertHomeCenterTop    = Position{i: 3, j: 9}
	DesertHomeCenterBottom = Position{i: 4, j: 9}
	DesertHomeBottom       = Position{i: 5, j: 9}
)

func MaxI() int {
	if IsPart1 {
		return 5
	} else {
		return 7
	}
}

func AmbarHome() map[Position]bool {
	if IsPart1 {
		return map[Position]bool{
			AmbarHomeTop:       true,
			AmbarHomeCenterTop: true,
		}
	} else {
		return map[Position]bool{
			AmbarHomeTop:          true,
			AmbarHomeCenterTop:    true,
			AmbarHomeCenterBottom: true,
			AmbarHomeBottom:       true,
		}
	}
}

func BronzeHome() map[Position]bool {
	if IsPart1 {
		return map[Position]bool{
			BronzeHomeTop:       true,
			BronzeHomeCenterTop: true,
		}
	}
	return map[Position]bool{
		BronzeHomeTop:          true,
		BronzeHomeCenterTop:    true,
		BronzeHomeCenterBottom: true,
		BronzeHomeBottom:       true,
	}
}

func CopperHome() map[Position]bool {
	if IsPart1 {
		return map[Position]bool{
			CopperHomeTop:       true,
			CopperHomeCenterTop: true,
		}
	}
	return map[Position]bool{
		CopperHomeTop:          true,
		CopperHomeCenterTop:    true,
		CopperHomeCenterBottom: true,
		CopperHomeBottom:       true,
	}
}

func DesertHome() map[Position]bool {
	if IsPart1 {
		return map[Position]bool{
			DesertHomeTop:       true,
			DesertHomeCenterTop: true,
		}
	}
	return map[Position]bool{
		DesertHomeTop:          true,
		DesertHomeCenterTop:    true,
		DesertHomeCenterBottom: true,
		DesertHomeBottom:       true,
	}
}

type World struct {
	Grid map[Position]byte
}

func (w World) String(as [NA]Agent) (s string) {
	s = ""
	for i := 0; i < MaxI(); i++ {
		for j := 0; j < 13; j++ {
			p := Position{i: i, j: j}
			agentin := false
			for _, a := range as {
				if a.Pos == p {
					s += string(a.Type)
					agentin = true
					break
				}
			}
			if !agentin {
				s += string(w.Grid[p])
			}
		}
		s += "\n"
	}
	return
}

func (w World) Print(as [NA]Agent) {
	fmt.Println(w.String(as))
}

func (w World) CanGoTo(a Agent, to Position, as [NA]Agent) bool {
	content, found := w.Grid[to]
	if !found || content == '#' || content == ' ' {
		return false
	}

	if content == '.' {
		if a.HasAlreadyMovedToHw {
			return false
		}

		// Cannot go in front of a home
		toDown := Position{i: to.i + 1, j: to.j}
		if toDown == AmbarHomeTop || toDown == BronzeHomeTop ||
			toDown == CopperHomeTop || toDown == DesertHomeTop {
			return false
		}

		if IsAnyAgentThere(to, a, as) {
			return false
		}

		return true
	} else {
		// Cannot in another home
		if content == 'a' && a.Type != Ambar || content == 'b' && a.Type != Bronze ||
			content == 'c' && a.Type != Copper || content == 'd' && a.Type != Desert {
			return false
		}

		if IsAnyAgentThere(to, a, as) {
			return false
		}
		// At this point the home is valid we still have to check if there are other
		// amphiboids in
		var home map[Position]bool
		if content == 'a' {
			home = AmbarHome()
		} else if content == 'b' {
			home = BronzeHome()
		} else if content == 'c' {
			home = CopperHome()
		} else {
			home = DesertHome()
		}
		for _, oa := range as {
			if oa != a {
				_, found := home[oa.Pos]
				if found && oa.Type != a.Type {
					return false
				}
			}
		}
		return true
	}
}

func (w World) CanTraceToHome(a Agent, as [NA]Agent, to Position) bool {
	if !w.CanGoTo(a, to, as) {
		return false
	}

	if a.Pos == to {
		return true
	}

	if a.Pos.i != 1 {
		for _, oa := range as {
			if a != oa && oa.Pos.j == a.Pos.j && oa.Pos.i >= 1 && oa.Pos.i <= a.Pos.i {
				return false
			}
		}
	}

	if a.Pos.j != to.j {
		minj := int(int(math.Min(float64(a.Pos.j), float64(to.j))))
		maxj := int(int(math.Max(float64(a.Pos.j), float64(to.j))))

		for _, oa := range as {
			if a != oa && oa.Pos.i == 1 && oa.Pos.j >= minj && oa.Pos.j <= maxj {
				return false
			}
		}
	}

	for _, oa := range as {
		if a != oa && oa.Pos.j == to.j && oa.Pos.i >= 1 && oa.Pos.i < to.i {
			return false
		}
	}

	return true
}

func IsAnyAgentThere(p Position, me Agent, as [NA]Agent) bool {
	for _, a := range as {
		if a != me && a.Pos == p {
			return true
		}
	}
	return false
}

func WhoIsThere(p Position, as [NA]Agent) *Agent {
	for _, a := range as {
		if a.Pos == p {
			return &a
		}
	}
	return nil
}

func UpdateAgent(oa, na Agent, as [NA]Agent) [NA]Agent {
	nas := as
	for i, a := range as {
		if a.Id == oa.Id {
			nas[i] = na
		}
	}
	return nas
}

func (w World) GetRespectiveHome(a Agent) map[Position]bool {
	if a.Type == Ambar {
		return AmbarHome()
	} else if a.Type == Bronze {
		return BronzeHome()
	} else if a.Type == Copper {
		return CopperHome()
	} else {
		return DesertHome()
	}
}

func (w World) IsCompleted(a Agent, as [NA]Agent) bool {
	home := w.GetRespectiveHome(a)
	c := 0
	for _, oa := range as {
		_, f := home[oa.Pos]
		if f && oa.Type == a.Type {
			c++
			if c == 4 {
				return true
			}
		}
	}
	return false
}

func (w World) IsPartiallyCompleted(a Agent, as [NA]Agent) bool {
	home := w.GetRespectiveHome(a)
	_, isin := home[a.Pos]
	if isin {
		for i := a.Pos.i + 1; i < MaxI()-1; i++ {
			oa := WhoIsThere(Position{i: i, j: a.Pos.j}, as)
			if oa == nil || oa.Type != a.Type {
				return false
			}
		}
		return true
	} else {
		return false
	}
}

func (w World) Expand(a Agent, as [NA]Agent) []Position {

	if w.IsCompleted(a, as) {
		return []Position{}
	}

	if w.IsPartiallyCompleted(a, as) {
		return []Position{}
	}

	var homeHor int
	if a.Type == Ambar {
		homeHor = 3
	} else if a.Type == Bronze {
		homeHor = 5
	} else if a.Type == Copper {
		homeHor = 7
	} else {
		homeHor = 9
	}

	for i := MaxI() - 2; i > 1; i-- {
		if w.CanTraceToHome(a, as, Position{i: i, j: homeHor}) {
			return []Position{{i: i, j: homeHor}}
		}
	}

	if a.HasAlreadyMovedToHw {
		return []Position{a.Pos}
	}

	visited := make(map[Position]bool)
	return w.expand(a.Pos, a, as, visited)
}

func (w World) expand(p Position, a Agent, as [NA]Agent, visited map[Position]bool) []Position {
	var vp []Position

	_, v := visited[p]
	if v {
		return vp
	}

	visited[p] = true
	if w.Grid[p] == '#' || w.Grid[p] == ' ' {
		return vp
	}

	if IsAnyAgentThere(p, a, as) {
		return vp
	}

	if a.Type == Ambar && p == AmbarHomeBottom ||
		a.Type == Bronze && p == BronzeHomeBottom ||
		a.Type == Copper && p == CopperHomeBottom ||
		a.Type == Desert && p == DesertHomeBottom {
		return []Position{p}
	}

	if w.IsPartiallyCompleted(a.Copy(p), as) {
		return []Position{p}
	}

	if w.CanGoTo(a, p, as) {
		vp = append(vp, p)
	}

	right := Position{i: p.i, j: p.j + 1}
	left := Position{i: p.i, j: p.j - 1}
	if p.i != 1 {
		cangoup := true
		for _, oa := range as {
			if a != oa && oa.Pos.j == p.j && oa.Pos.i <= p.i {
				cangoup = false
				break
			}
		}
		if cangoup {
			vp = append(vp, w.expand(Position{i: 1, j: p.j}, a, as, visited)...)
		}
	}

	vp = append(vp, w.expand(right, a, as, visited)...)
	vp = append(vp, w.expand(left, a, as, visited)...)

	// Go way down!
	if a.Pos.i == 1 {
		mini := 4
		for _, oa := range as {
			if oa != a && oa.Pos.j == p.j && oa.Pos.i < mini {
				mini = oa.Pos.i
			}
		}
		dp := Position{i: mini - 1, j: p.j}
		_, v := visited[dp]
		if !v && w.CanGoTo(a, dp, as) {
			vp = append(vp, dp)
		}
	}

	return vp
}

func (w World) IsGoal(as [NA]Agent) bool {
	for _, a := range as {
		home := w.GetRespectiveHome(a)
		_, found := home[a.Pos]
		if !found {
			return false
		}
	}
	return true
}

type Agent struct {
	Id                  int
	Type                byte
	Pos                 Position
	HasAlreadyMovedToHw bool
}

var AgentId int

func NewAgent(t byte, pos Position) Agent {
	AgentId++
	return Agent{
		Id:                  AgentId,
		Type:                t,
		Pos:                 pos,
		HasAlreadyMovedToHw: false,
	}
}

func (a Agent) Copy(np Position) Agent {
	return Agent{
		Id:                  a.Id,
		Type:                a.Type,
		Pos:                 np,
		HasAlreadyMovedToHw: a.HasAlreadyMovedToHw,
	}
}

func (a Agent) Energy() int {
	return map[byte]int{
		Ambar:  1,
		Bronze: 10,
		Copper: 100,
		Desert: 1000,
	}[a.Type]
}

type AmphoidRoom struct {
	W  World
	As [NA]Agent
}

func ReadAmphoidRoom(fpath string) AmphoidRoom {
	f, err := os.Open(fpath)
	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(f)
	var i int
	var as [NA]Agent
	var ai int
	grid := make(map[Position]byte)
	for scanner.Scan() {
		for j, c := range scanner.Text() {
			p := Position{i: i, j: j}
			if c == '\n' {
				continue
			}
			_, amh := AmbarHome()[p]
			_, brh := BronzeHome()[p]
			_, coh := CopperHome()[p]
			_, dsh := DesertHome()[p]

			if amh {
				grid[p] = 'a'
			} else if brh {
				grid[p] = 'b'
			} else if coh {
				grid[p] = 'c'
			} else if dsh {
				grid[p] = 'd'
			} else {
				grid[p] = byte(c)
				if i == 1 && c != '#' {
					grid[p] = '.'
				}
			}

			if c == 'A' || c == 'B' || c == 'C' || c == 'D' {
				as[ai] = NewAgent(byte(c), p)
				ai++
			}
		}
		i++
	}
	return AmphoidRoom{W: World{Grid: grid}, As: as}
}

func Manhattan(start, end Position) int {
	return int(math.Abs(float64(start.i-end.i)) + math.Abs(float64(start.j-end.j)))
}

type AStarNode struct {
	G      int
	H      int
	Parent *AStarNode
	State  [NA]Agent
}

func (asp AStarNode) F() int {
	return asp.G + asp.H
}

type PriorityQueue []AStarNode

func SortedInsert(s *PriorityQueue, n AStarNode) {
	if len(*s) == 0 {
		(*s) = append(*s, n)
	}
	i := sort.Search(len(*s), func(i int) bool { return (*s)[i].F() > n.F() })
	(*s) = append(*s, AStarNode{})
	copy((*s)[i+1:], (*s)[i:])
	(*s)[i] = n
}

func (pq *PriorityQueue) Contains(v [NA]Agent) int {
	for i, n := range *pq {
		if n.State == v {
			return i
		}
	}
	return -1
}

func (pq *PriorityQueue) Add(v [NA]Agent, g, h int, parent *AStarNode) {
	idx := pq.Contains(v)
	if idx != -1 {
		log.Fatalf("Duplicated register %+v...", v)
	}
	SortedInsert(pq, AStarNode{State: v, Parent: parent, G: g, H: h})
}

func (pq *PriorityQueue) GetPriority(p [NA]Agent) AStarNode {
	idx := pq.Contains(p)
	if idx == -1 {
		log.Fatal("GetPriority on a non existent point")
	}
	return (*pq)[idx]
}

func (pq *PriorityQueue) Deque() AStarNode {
	elem := (*pq)[0]
	(*pq) = (*pq)[1:]
	return elem
}

func (pq *PriorityQueue) UpdatePriority(state [NA]Agent, g, h int, parent *AStarNode) {
	idx := pq.Contains(state)
	if idx == -1 {
		log.Fatal("Update priority on a non existent point")
	}
	(*pq)[idx] = AStarNode{G: g, H: h, Parent: parent, State: state}
	sort.SliceStable(*pq, func(i, j int) bool {
		return (*pq)[i].F() < (*pq)[j].F()
	})
}

func ComputeHeuristic(w World, as [NA]Agent) (h int) {
	for _, a := range as {
		if w.IsPartiallyCompleted(a, as) {
			continue
		}
		m1 := Manhattan(a.Pos, Position{i: 1, j: a.Pos.j})
		if a.Type == Ambar {
			h += m1 + Manhattan(Position{i: 1, j: a.Pos.j}, AmbarHomeBottom)
		} else if a.Type == Copper {
			h += m1 + Manhattan(Position{i: 1, j: a.Pos.j}, CopperHomeBottom)
		} else if a.Type == Bronze {
			h += m1 + Manhattan(Position{i: 1, j: a.Pos.j}, BronzeHomeBottom)
		} else {
			h += m1 + Manhattan(Position{i: 1, j: a.Pos.j}, DesertHomeBottom)
		}
	}
	return
}

func UpdatePQ(pq *PriorityQueue, cn AStarNode, w World, a Agent, np Position, visited map[[NA]Agent]bool) {
	as := cn.State
	na := a.Copy(np)
	if np.i == 1 {
		na.HasAlreadyMovedToHw = true
	}
	child := UpdateAgent(a, na, as)
	_, av := visited[child]
	if av {
		return
	}

	g := cn.G +
		(Manhattan(a.Pos, Position{i: 1, j: a.Pos.j})+Manhattan(Position{i: 1, j: a.Pos.j}, na.Pos))*a.Energy()
	h := ComputeHeuristic(w, child)
	idx := pq.Contains(child)
	if idx != -1 && pq.GetPriority(child).G > g {
		pq.UpdatePriority(child, g, h, &cn)
	} else if idx == -1 {
		pq.Add(child, g, h, &cn)
	}
}

func AStar(w World, as [NA]Agent) {
	pq := make(PriorityQueue, 0)
	visited := make(map[[NA]Agent]bool)

	pq.Add(as, 0, ComputeHeuristic(w, as), nil)

	for len(pq) != 0 {
		cn := pq.Deque()
		cas := cn.State
		visited[cas] = true
		if w.IsGoal(cas) {
			w.Print(cas)
			parent := cn.Parent
			var tc int
			for parent != nil {
				var cost int
				for i, a := range cas {
					if a.Pos != parent.State[i].Pos {
						cost += (Manhattan(a.Pos, Position{i: 1, j: a.Pos.j}) +
							Manhattan(Position{i: 1, j: a.Pos.j}, parent.State[i].Pos)) *
							a.Energy()
					}
				}
				tc += cost
				cas = parent.State
				w.Print(parent.State)
				parent = parent.Parent
			}
			fmt.Println("Final cost: ", tc)
			break
		} else {
			for _, a := range cas {
				if a.Pos.i == 1 {
					a.HasAlreadyMovedToHw = true
				}
				nextPositions := w.Expand(a, cas)
				for _, np := range nextPositions {
					UpdatePQ(&pq, cn, w, a, np, visited)
				}
			}
		}
	}
}

func main() {
	aw := ReadAmphoidRoom("input.txt")
	AStar(aw.W, aw.As)
}
