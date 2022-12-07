import abc
from dataclasses import dataclass
from typing import List, Mapping


@dataclass
class Sizeable(abc.ABC):
    path: str

    @abc.abstractmethod
    def size(self) -> int:
        ...


@dataclass
class File(Sizeable):
    size_: int

    def size(self) -> int:
        return self.size_


@dataclass
class Directory(Sizeable):
    nested: List[Sizeable]

    def size(self) -> int:
        return sum(o.size() for o in self.nested)


def read_puzzle_input(fname: str) -> Directory:
    with open(fname) as f:
        cmd_output = [o.strip() for o in f]

    root = Directory(path="/", nested=[])
    directories = {"/": root}
    dir_stack = [root]

    for line in cmd_output[1:]:
        current_path = "/".join(d.path for d in dir_stack)
        if line.startswith("$ cd .."):
            dir_stack.pop(-1)
        elif line.startswith("$ cd"):
            name = line.removeprefix("$ cd ")
            dir_stack.append(directories[current_path + "/" + name])
        elif not line.startswith("$ ls"):
            info, name = line.split(" ")
            path = current_path + "/" + name
            peek = dir_stack[-1]
            if info == "dir":
                if path not in directories:
                    directories[path] = Directory(path=path, nested=[])

                peek.nested.append(directories[path])
            else:
                peek.nested.append(File(path=path, size_=int(info)))
    return root


def compute_dir_sizes(root: Directory) -> Mapping[str, int]:
    dir_sizes = {root.path: root.size()}
    queue: List[Directory] = [root]

    while queue:
        m = queue.pop(0)
        dir_sizes[m.path] = m.size()
        for child in m.nested:
            if child.path not in dir_sizes and isinstance(child, Directory):
                queue.append(child)

    return dir_sizes


def select_dirs_to_remove(
    dir_sizes: Mapping[str, int],
    total_disk_space: int = 70000000,
    needed_space: int = 30000000,
) -> str:
    unused = total_disk_space - dir_sizes["/"]
    big_enough = [k for k, v in dir_sizes.items() if unused + v > needed_space]
    return min(big_enough, key=dir_sizes.__getitem__)


def main() -> None:
    dirtree = read_puzzle_input("puzzle-input.txt")
    dir_sizes = compute_dir_sizes(dirtree)
    print("Part 1 solution:", sum(v for v in dir_sizes.values() if v <= 100000))
    print("Part 2 solution:", dir_sizes[select_dirs_to_remove(dir_sizes)])


if __name__ == "__main__":
    main()
