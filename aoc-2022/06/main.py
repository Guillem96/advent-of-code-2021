import more_itertools


def find_start_marker(signal: str, marker_size: int) -> int:
    for i, wdw in enumerate(more_itertools.windowed(signal, marker_size)):
        if len(set(wdw)) == len(wdw):
            return marker_size + i 

def main() -> None:
    with open("puzzle-input.txt") as f:
        signal = f.read().strip()

    print("Part 1 solution:", find_start_marker(signal, 4))
    print("Part 2 solution:", find_start_marker(signal, 14))


if __name__ == "__main__":
    main()
