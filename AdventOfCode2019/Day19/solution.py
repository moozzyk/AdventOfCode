import sys
from intcode import Intcode


def in_beam(program, x, y):
    cpu = Intcode(program)
    cpu.input.extend((x, y))
    cpu.run()
    return cpu.output.pop(0) != 0


def problem1(program):
    count = 0
    for x in range(0, 50):
        for y in range(0, 50):
            if in_beam(program, x, y):
                count += 1
    print(count)


def track_beam(program, size=100):
    top, right = 10, 10
    while True:
        top += 1
        while in_beam(program, right + 1, top):
            right += 1
        if right > size and in_beam(program, right - size + 1, top + size - 1):
            return (top, right - size + 1)


def problem2(program):
    (y, x) = track_beam(program)
    print(x * 10000 + y)


with open(sys.argv[1], "r") as f:
    program = list(map(lambda n: int(n), f.readline().split(",")))
    problem1(program)
    problem2(program)
