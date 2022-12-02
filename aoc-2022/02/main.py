import enum
from typing import List, NamedTuple, Type, Union


class Outcome(enum.Enum):
    WIN: str = "win"
    LOOSE: str = "loose"
    DRAW: str = "draw"


class Hand(enum.Enum):
    ROCK: str = "rock"
    PAPER: str = "paper"
    SCISSORS: str = "scissors"

    @classmethod
    def from_char(cls, char: str) -> "Hand":
        if char == "A" or char == "X":
            return Hand.ROCK

        if char == "B" or char == "Y":
            return Hand.PAPER

        if char == "C" or char == "Z":
            return Hand.SCISSORS

        raise ValueError(f"invalid char {char} for Hand")

    def is_win(self, other: "Hand") -> bool:
        return (
            self == Hand.ROCK
            and other == Hand.SCISSORS
            or self == Hand.SCISSORS
            and other == Hand.PAPER
            or self == Hand.PAPER
            and other == Hand.ROCK
        )

    def result(self, other: "Hand") -> Outcome:
        if self == other:
            return Outcome.DRAW

        if self.is_win(other):
            return Outcome.WIN

        return Outcome.LOOSE


class Round(NamedTuple):
    elf: Hand
    me: Hand

    @classmethod
    def from_line(cls, line: str) -> "Round":
        elf, me = [Hand.from_char(c) for c in line.strip().split()]
        return cls(elf, me)

    def score(self) -> int:
        if self.me == Hand.ROCK:
            base = 1
        elif self.me == Hand.PAPER:
            base = 2
        else:
            base = 3

        if self.me.result(self.elf) == Outcome.WIN:
            return base + 6

        if self.me.result(self.elf) == Outcome.DRAW:
            return base + 3

        return base


LOOSE_MAPPING = {
    Hand.ROCK: Hand.SCISSORS,
    Hand.PAPER: Hand.ROCK,
    Hand.SCISSORS: Hand.PAPER,
}
WIN_MAPPING = {v: k for k, v in LOOSE_MAPPING.items()}


class RoundPart2(NamedTuple):
    elf: Hand
    me: Hand

    @classmethod
    def from_line(cls, line: str) -> "RoundPart2":
        elf_s, me_s = line.strip().split()
        elf = Hand.from_char(elf_s)
        if me_s == "X":
            return cls(elf, LOOSE_MAPPING[elf])
        if me_s == "Z":
            return cls(elf, WIN_MAPPING[elf])

        return cls(elf, elf)

    def score(self) -> int:
        if self.me == Hand.ROCK:
            base = 1
        elif self.me == Hand.PAPER:
            base = 2
        else:
            base = 3

        if self.me.result(self.elf) == Outcome.WIN:
            return base + 6

        if self.me.result(self.elf) == Outcome.DRAW:
            return base + 3

        return base


def read_input(
    fname: str,
    round_cls: Union[Type[Round], Type[RoundPart2]],
) -> List[Union[Round, RoundPart2]]:
    with open(fname) as f:
        return [round_cls.from_line(l) for l in f if l]


def main() -> None:
    rounds_part_1 = read_input("puzzle-input.txt", Round)
    rounds_part_2 = read_input("puzzle-input.txt", RoundPart2)

    print("Part 1 solution:", sum(o.score() for o in rounds_part_1))
    print("Part 2 solution:", sum(o.score() for o in rounds_part_2))


if __name__ == "__main__":
    main()
