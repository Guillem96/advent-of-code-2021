from typing import List, Sequence, Tuple

ElfPair = Tuple[range, range]


def read_puzzle_input(fname: str) -> Sequence[ElfPair]:
    res: List[ElfPair] = []
    with open(fname) as f:
        for line in f:
            section_1, section_2 = line.strip().split(",")
            start_1, end_1 = [int(o) for o in section_1.split("-")]
            start_2, end_2 = [int(o) for o in section_2.split("-")]
            res.append((range(start_1, end_1 + 1), range(start_2, end_2 + 1)))
    return res


def fully_overlap(ep: ElfPair) -> bool:
    pos1, pos2 = [set(o) for o in ep]
    return pos1.issubset(pos2) or pos2.issubset(pos1)


def partially_overlap(ep: ElfPair) -> bool:
    pos1, pos2 = [set(o) for o in ep]
    return bool(pos1.intersection(pos2))


def main() -> None:
    assignments = read_puzzle_input("puzzle-input.txt")
    print("Part 1 solution:", sum(fully_overlap(o) for o in assignments))
    print("Part 2 solution:", sum(partially_overlap(o) for o in assignments))


if __name__ == "__main__":
    main()
