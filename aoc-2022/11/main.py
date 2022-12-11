import copy
import math
import operator
from dataclasses import dataclass
from typing import Callable, List, Optional, Sequence, Tuple, Union

Operation = Callable[[int], int]
Test = Callable[[int], int]


@dataclass
class Monkey:
    items: List[int]
    op: Operation
    test: Test
    _items_inspected: int = 0

    @property
    def inspected_items(self) -> int:
        return self._items_inspected

    def inspect_item(self, post_fn: Operation) -> Optional[Tuple[int, int]]:
        if not self.items:
            return None
        self._items_inspected += 1
        ci = self.items.pop(0)
        transformed_item = post_fn(self.op(ci))
        next_monkey = self.test(transformed_item)
        return transformed_item, next_monkey


def create_op(l: str) -> Operation:
    if "+" in l:
        operators = l.split(" + ")

        def op(old: int) -> int:
            return eval(operators[0]) + eval(operators[1])

    else:
        operators = l.split(" * ")

        def op(old: int) -> int:
            return eval(operators[0]) * eval(operators[1])

    return op


def create_test(divisible_by: int, true_value: int, false_value: int) -> Operation:
    def op(x: int) -> int:
        return true_value if x % divisible_by == 0 else false_value

    return op


def read_puzzle_input(fname: str) -> Tuple[Sequence[Monkey], int]:
    with open(fname) as f:
        content = f.read().split("\n")

    monkeys = []
    all_divisible_by = []
    starting_items: List[int]
    current_operation: str
    if_false_to_monkey: int
    if_true_to_monkey: int
    divisible_by: int

    for l in content[1:]:
        l = l.strip()
        if l.startswith("Monkey"):
            monkeys.append(
                Monkey(
                    items=starting_items,
                    op=create_op(current_operation),
                    test=create_test(
                        divisible_by, if_true_to_monkey, if_false_to_monkey,
                    ),
                ),
            )
        elif l.startswith("Starting items:"):
            starting_items = [
                int(o) for o in l.removeprefix("Starting items: ").split(",")
            ]
        elif l.startswith("Operation: "):
            l = l.removeprefix("Operation: new = ")
            current_operation = l
        elif l.startswith("Test: divisible by "):
            l = l.removeprefix("Test: divisible by ")
            divisible_by = int(l)
            all_divisible_by.append(divisible_by)
        elif l.startswith("If true: throw to monkey"):
            l = l.removeprefix("If true: throw to monkey")
            if_true_to_monkey = int(l)
        elif l.startswith("If false: throw to monkey"):
            l = l.removeprefix("If false: throw to monkey")
            if_false_to_monkey = int(l)

    monkeys.append(
        Monkey(
            items=starting_items,
            op=create_op(current_operation),
            test=create_test(divisible_by, if_true_to_monkey, if_false_to_monkey),
        ),
    )
    return monkeys, math.lcm(*all_divisible_by)


def compute_business(monkeys: Sequence[Monkey], rounds: int, post_fn: Operation) -> int:
    monkeys = copy.deepcopy(monkeys)
    for _ in range(rounds):
        for i in range(len(monkeys)):
            m = monkeys[i]
            while (inspected := m.inspect_item(post_fn)) is not None:
                transformed_item, next_monkey = inspected
                monkeys[next_monkey].items.append(transformed_item)
    n_inspected = [m.inspected_items for m in monkeys]
    return operator.mul(*sorted(n_inspected, reverse=True)[:2])


def part1_post_fn(x: int) -> int:
    return math.floor(x / 3)


def part1(monkeys: Sequence[Monkey]) -> int:
    return compute_business(monkeys, 20, part1_post_fn)


def part2_post_fn(lcm: int) -> Operation:
    def post(x: int) -> int:
        return x % lcm

    return post


def part2(monkeys: Sequence[Monkey], lcm: int) -> int:
    return compute_business(monkeys, 10000, part2_post_fn(lcm))


def main() -> None:
    monkeys, lcm = read_puzzle_input("puzzle-input.txt")
    print("Part 1 solution:", part1(copy.deepcopy(monkeys)))
    print("Part 2 solution:", part2(monkeys, lcm))


if __name__ == "__main__":
    main()
