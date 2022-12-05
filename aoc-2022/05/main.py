import collections
import copy
import re
from typing import Dict, List, NamedTuple, Tuple

import more_itertools

Stack = List[str]


class Movement(NamedTuple):
    quantity: int
    from_: int
    to: int

    @classmethod
    def from_line(cls, line: str) -> "Movement":
        m = re.match(r"move (?P<quantity>\d+) from (?P<from_>\d+) to (?P<to>\d+)", line)
        if m is None:
            raise ValueError()

        return cls(**{k: int(v) for k, v in m.groupdict().items()})


def read_puzzle_input(fname: str) -> Tuple[Dict[int, Stack], List[Movement]]:
    with open(fname) as f:
        lines = [l.removesuffix("\n") for l in f if l.strip()]

    stacks = collections.defaultdict(list)
    stacks_it, movement_it = more_itertools.split_at(
        lines,
        lambda l: l.startswith(" 1"),
    )
    more_itertools.consume(movement_it, 1)

    for stack_row in stacks_it:
        for si, letter in enumerate(more_itertools.chunked(stack_row, 4), start=1):
            # letters always have this structure ['[', 'L', ']', ' ' ]
            if letter[1] != " ":
                stacks[si].append(letter[1])

    movements = [Movement.from_line(l) for l in movement_it]
    return stacks, movements


def move_creates(
    stacks: Dict[int, Stack],
    movements: List[Movement],
    is_9001: bool = False,
) -> str:
    for m in movements:
        crates = stacks[m.from_][: m.quantity]
        stacks[m.from_] = stacks[m.from_][m.quantity :]
        if is_9001:
            stacks[m.to] = crates + stacks[m.to]
        else:
            stacks[m.to] = crates[::-1] + stacks[m.to]

    return "".join(stacks[i][0] for i in range(1, len(stacks) + 1))


def main() -> None:
    stacks, movements = read_puzzle_input("puzzle-input.txt")
    print("Part 1 solution:", move_creates(copy.deepcopy(stacks), movements))
    print(
        "Part 2 solution:",
        move_creates(copy.deepcopy(stacks), movements, is_9001=True),
    )


if __name__ == "__main__":
    main()
