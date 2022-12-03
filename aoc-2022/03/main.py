import functools
import string
from typing import Iterator, List, Set, Tuple, TypeVar

T = TypeVar("T")
Rucksack = Tuple[Set[str], Set[str]]


def read_puzzle_input(fname: str) -> List[Rucksack]:
    rss: List[Rucksack] = []

    with open(fname) as f:
        for l in f:
            sl = l.strip()
            mid = len(sl) // 2
            rss.append((set(sl[:mid]), set(sl[mid:])))
    return rss


def batchify(it: List[T], batch_size: int) -> Iterator[List[T]]:
    for i in range(0, len(it), batch_size):
        yield it[i : i + batch_size]


def single_element(s: Set[T]) -> T:
    if len(s) != 1:
        raise ValueError("Invalid set for single_element.")

    return next(iter(s))


def intersect_reduce(acc: Set[str], r: Rucksack) -> Set[str]:
    return acc.intersection(r[0].union(r[1]))


def main() -> None:
    letter_score = {l: i for i, l in enumerate(string.ascii_letters, start=1)}
    rucksacks = read_puzzle_input("puzzle-input.txt")

    part1_res = 0
    for rs in rucksacks:
        part1_res += letter_score[single_element(rs[0].intersection(rs[1]))]

    print("Part 1 solution:", part1_res)

    part2_res = 0
    for rs_batch in batchify(rucksacks, 3):
        badge = single_element(
            functools.reduce(intersect_reduce, rs_batch, set(letter_score)),
        )
        part2_res += letter_score[badge]

    print("Part 2 solution:", part2_res)


if __name__ == "__main__":
    main()
