from intcode import Intcode
import sys


def get_new_position(position, next_move):
    x, y = position
    if next_move == 1:
        position = (x, y - 1)
    elif next_move == 2:
        position = (x, y + 1)
    elif next_move == 3:
        position = (x - 1, y)
    elif next_move == 4:
        position = (x + 1, y)
    else:
        assert("Invalid move: " + next_move)

    return position


def build_map(cpu, position, m):
    reverse_moves = [0, 2, 1, 4, 3]
    elements = ["#", ".", "O"]
    for next_move in range(1, 5):
        new_position = get_new_position(position, next_move)
        if new_position in m.keys():
            continue
        cpu.input.append(next_move)
        cpu.run()
        result = cpu.output.pop(0)
        m[new_position] = elements[result]
        if result != 0:
            build_map(cpu, new_position, m)
            cpu.input.append(reverse_moves[next_move])
            cpu.run()
            cpu.output.pop()


def print_map(m):
    print(len(m))
    min_x, min_y, max_x, max_y = 0, 0, 0, 0
    for position in m.keys():
        min_x = min(min_x, position[0])
        min_y = min(min_y, position[1])
        max_x = max(max_x, position[0])
        max_y = max(max_y, position[1])

    screen = [['_'] * (max_x - min_x + 1) for _ in range(min_y, max_y + 1)]
    for item in m.items():
        ((x, y), element) = item
        screen[y - min_y][x - min_x] = element

    for l in screen:
        print("".join(l))


def find_oxygen(m):
    queue = [((0, 0), 0)]
    visited = set()
    while queue:
        (position, path_length) = queue.pop(0)
        if position in visited or position not in m.keys():
            continue
        visited.add(position)
        if m[position] == 'O':
            return (path_length, position)
        if m[position] == '#':
            continue
        for next_move in range(1, 5):
            queue.append(
                (get_new_position(position, next_move), path_length + 1))


def oxygenize(m, start_position):
    time = 0
    queue = [start_position]
    while True:
        new_queue = []
        while queue:
            position = queue.pop(0)
            m[position] = 'O'
            for next_move in range(1, 5):
                new_position = get_new_position(position, next_move)
                if m[new_position] == ".":
                    new_queue.append(new_position)
        if not new_queue:
            return time
        queue = new_queue
        time += 1


def problem1(program):
    m = {}
    build_map(Intcode(list(program)), (0, 0), m)
    m[(0, 0)] = "*"
    print(find_oxygen(m))


def problem2(program):
    m = {}
    build_map(Intcode(list(program)), (0, 0), m)
    _, oxygen_position = find_oxygen(m)
    print(oxygenize(m, oxygen_position))


with open(sys.argv[1], "r") as f:
    program = list(map(lambda n: int(n), f.readline().split(",")))
    problem1(program)
    problem2(program)