import sys
from intcode import Intcode


def create_map(program):
    cpu = Intcode(program, [], [])
    cpu.run()
    converted_output = "".join(list(map(chr, cpu.output))).split("\n")
    return list(filter(lambda c: c != "", converted_output))


def problem1(program):
    alignment_sum = 0
    view = create_map(program)
    for row in range(1, len(view) - 1):
        for col in range(1, len(view[row]) - 1):
            dirs = [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)]
            if all(map(lambda d: view[row + d[0]][col + d[1]] == "#", dirs)):
                alignment = row * col
                alignment_sum += alignment
    print(alignment_sum)


def find_starting_point(scaffolding):
    for row in range(len(scaffolding)):
        for col in range(len(scaffolding[row])):
            if scaffolding[row][col] in "^v<>":
                return (row, col, scaffolding[row][col])
    assert("Could not find robot")


def move_forward(row, col, direction, scaffolding):
    dirs = {"^": (-1, 0), "v": (1, 0), "<": (0, -1), ">": (0, 1)}
    (d_row, d_col) = dirs[direction]
    return (row + d_row, col + d_col)


def can_move_forward(row, col, direction, scaffolding):
    (row, col) = move_forward(row, col, direction, scaffolding)
    return row >= 0 and col >= 0 and row < len(scaffolding) and \
        col < len(scaffolding[row]) and scaffolding[row][col] == "#"


def turn(row, col, direction, scaffolding):
    directions = "<^>v"
    current = directions.index(direction)
    new_dir = directions[(current - 1) % len(directions)]
    if can_move_forward(row, col, new_dir, scaffolding):
        return (new_dir, "L")
    new_dir = directions[(current + 1) % len(directions)]
    if (can_move_forward(row, col, new_dir, scaffolding)):
        return (new_dir, "R")
    return (" ", "*")


def create_path(scaffolding, starting_point):
    (row, col, direction), path = starting_point, []
    while True:
        steps = 0
        while can_move_forward(row, col, direction, scaffolding):
            (row, col) = move_forward(row, col, direction, scaffolding)
            steps += 1

        if steps > 0:
            path.append(str(steps))

        (direction, t) = turn(row, col, direction, scaffolding)
        if t == "*":
            break
        path.append(t)
    return path


def calculate_dust(program):
    main = "A,B,A,B,C,C,B,A,B,C\n"
    A = "L,10,R,10,L,10,L,10\n"
    B = "R,10,R,12,L,12\n"
    C = "R,12,L,12,R,6\n"
    video = "n\n"
    input_lines = [main, A, B, C, video]
    program[0] = 2
    cpu = Intcode(program, [], [])
    cpu.input.extend(map(ord, "".join(input_lines)))
    cpu.run()
    return cpu.output[-1]


def problem2(program):
    scaffolding = create_map(program)
    start = find_starting_point(scaffolding)
    path = create_path(scaffolding, start)
    print(path)
    print(calculate_dust(program))


with open(sys.argv[1], "r") as f:
    program = list(map(lambda n: int(n), f.readline().split(",")))
    problem1(list(program))
    problem2(list(program))
