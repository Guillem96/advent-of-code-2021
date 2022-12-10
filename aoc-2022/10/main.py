import abc
from dataclasses import dataclass
from typing import Iterator, List, Sequence


@dataclass
class Operation(abc.ABC):
    @property
    @abc.abstractmethod
    def cycles(self) -> int:
        ...

    def __call__(self, other: int) -> int:
        return other


class NOOP(Operation):
    @property
    def cycles(self) -> int:
        return 1


@dataclass
class AddX(Operation):
    V: int

    @property
    def cycles(self) -> int:
        return 2

    def __call__(self, other: int) -> int:
        return other + self.V


def read_puzzle_input(fname: str) -> Sequence[Operation]:
    ops: List[Operation] = []
    with open(fname) as f:
        for line in f:
            op, *v = line.strip().split()
            if op == "noop":
                ops.append(NOOP())
            elif op == "addx":
                ops.append(AddX(int(v[0])))

    return ops


def compute_strength(ops: Sequence[Operation], relevant_cycles: Iterator[int]) -> int:
    x = 1
    signal_strength = 0
    current_cycle = 0
    current_relevant_cycle = next(relevant_cycles)

    for o in ops:
        current_cycle += o.cycles
        if current_cycle >= current_relevant_cycle:
            signal_strength += x * current_relevant_cycle
            try:
                current_relevant_cycle = next(relevant_cycles)
            except StopIteration:
                return signal_strength

        x = o(x)

    return signal_strength


def draw(ops: Sequence[Operation]) -> str:
    x = 1
    current_row = ["."] * 40
    current_draw_pixel = 0
    CRT = []
    for o in ops:
        for _ in range(o.cycles):
            if current_draw_pixel >= x - 1 and current_draw_pixel <= x + 1:
                current_row[current_draw_pixel] = "#"

            current_draw_pixel += 1
            if current_draw_pixel == 40:
                CRT.append("".join(current_row))
                current_draw_pixel = 0
                current_row = ["."] * 40

        x = o(x)

    return "\n".join(CRT)


def main() -> None:
    operations = read_puzzle_input("puzzle-input.txt")
    print(
        "Signal Strength (Part 1):",
        compute_strength(operations, iter(range(20, 221, 40))),
    )
    print(draw(operations))


if __name__ == "__main__":
    main()
