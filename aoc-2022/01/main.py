from typing import List

ElfCalories = List[int]

def read_input(fname: str) -> List[ElfCalories]:
    all_elves = []
    current_elf = []
    with open(fname) as f:
        for l in f.readlines():
            if l == "\n":
                all_elves.append(current_elf)
                current_elf = []
            elif l.strip().isdigit():
                current_elf.append(int(l.strip()))

    all_elves.append(current_elf)
    return all_elves

    
def main() -> None:
    elves_calories = read_input("puzzle-input.txt")
    elves_sums = [sum(o) for o in elves_calories]
    print("Part 1:", max(elves_sums))
    print("Part 2 solution:", sum(sorted(elves_sums)[-3:]))


if __name__ == "__main__":
    main()
