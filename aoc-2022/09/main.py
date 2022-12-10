import enum
from dataclasses import dataclass, field
from typing import List, NamedTuple, Sequence, Set, Tuple


class Direction(enum.Enum):
    R = "R"
    U = "U"
    L = "L"
    D = "D"


class Movement(NamedTuple):
    direction: Direction
    qty: int


Position = Tuple[int, int]


@dataclass
class Rope:
    knots: List[Position]
    visited: Set[Position] = field(default_factory=set)

    def apply_movement(self, mv: Movement) -> None:
        for _ in range(mv.qty):
            # Move the head
            self.knots[0] = apply_direction(self.knots[0], mv.direction)

            # Make the knots follow the head and the following knot
            for i in range(len(self.knots) - 1):
                head = self.knots[i]
                tail = self.knots[i + 1]

                # Just add to visited the positions of the absolute tail
                if i + 1 == len(self.knots) - 1:
                    self.visited.add(tail)

                # If distance to the previous knot is larger that 2, move
                if distance(head, tail) >= 2:
                    d = tail_move(head, tail)
                    tail = (tail[0] + d[0], tail[1] + d[1])

                self.knots[i] = head
                self.knots[i + 1] = tail


def tail_move(hp: Position, tp: Position) -> Position:
    dx = hp[0] - tp[0]
    if dx != 0:
        dx = dx // abs(dx)

    dy = hp[1] - tp[1]
    if dy != 0:
        dy = dy // abs(dy)

    return dx, dy


def distance(pos1: Position, pos2: Position) -> float:
    return max([abs(pos1[i] - pos2[i]) for i in range(2)])


def apply_direction(position: Position, direction: Direction) -> Position:
    if direction == Direction.R:
        return (position[0] + 1, position[1])

    if direction == Direction.L:
        return (position[0] - 1, position[1])

    if direction == Direction.U:
        return (position[0], position[1] + 1)

    # D
    return (position[0], position[1] - 1)


def read_puzzle_input(fname: str) -> Sequence[Movement]:
    mvs = []
    with open(fname) as f:
        for line in f:
            dir_str, qty_s = line.strip().split()
            mvs.append(Movement(Direction(dir_str), int(qty_s)))

    return mvs


def main() -> None:
    movements = read_puzzle_input("puzzle-input.txt")
    state_part_1 = Rope([(0, 0)] * 2)  # 2 knots -> head and tail
    state_part_2 = Rope([(0, 0)] * 10)  # 10 knots -> head + 9 knots

    for m in movements:
        state_part_1.apply_movement(m)
        state_part_2.apply_movement(m)

    print("Part 1 solution:", len(state_part_1.visited))
    print("Part 2 solution:", len(state_part_2.visited))


if __name__ == "__main__":
    main()
