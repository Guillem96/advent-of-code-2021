import itertools
from typing import List

TreeGrid = List[List[int]]


def read_puzzle_input(fname: str) -> TreeGrid:
    with open(fname) as f:
        lines = [l.strip() for l in f]

    return [[int(o) for o in row] for row in lines]


def count_till_larger(items: List[int], comp: int) -> int:
    count = 0
    for item in items:
        count += 1
        if item >= comp:
            return count
    return count


def all_lower(items: List[int], comp: int) -> bool:
    return all(comp > rr for rr in items)


def scenic_score(x: int, y: int, tree_grid: TreeGrid) -> int:
    c = len(tree_grid)
    left_score = count_till_larger(tree_grid[y][:x][::-1], tree_grid[y][x])
    right_score = count_till_larger(tree_grid[y][x + 1 :], tree_grid[y][x])
    up_score = count_till_larger(
        [tree_grid[i][x] for i in list(range(y))[::-1]], tree_grid[y][x]
    )
    down_score = count_till_larger(
        [tree_grid[i][x] for i in range(y + 1, c)], tree_grid[y][x]
    )
    return right_score * left_score * up_score * down_score


def is_visible(x: int, y: int, tree_grid: TreeGrid) -> bool:
    r, c = len(tree_grid[0]), len(tree_grid)

    if x == 0 or x == r - 1 or y == 0 or y == c - 1:
        return True

    row_left = tree_grid[y][:x]
    row_right = tree_grid[y][x + 1:]

    column_up = [tree_grid[i][x] for i in range(y)]
    column_down = [tree_grid[i][x] for i in range(y + 1, c)]

    h = tree_grid[y][x]
    return (
        all_lower(row_left, h)
        or all_lower(row_right, h)
        or all_lower(column_up, h)
        or all_lower(column_down, h)
    )


def main() -> None:
    tree_grid = read_puzzle_input("puzzle-input.txt")
    count = 0
    scenic_scores = {}
    for x, y in itertools.product(range(len(tree_grid[0])), range(len(tree_grid))):
        scenic_scores[(y, x)] = scenic_score(x, y, tree_grid)
        if is_visible(x, y, tree_grid):
            count += 1

    print("Part 1 solution:", count)
    print("Part 2 solution:", max(scenic_scores.values()))


if __name__ == "__main__":
    main()
