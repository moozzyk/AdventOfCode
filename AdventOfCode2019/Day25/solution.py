from intcode import Intcode
import sys


def problem1(program, steps):
    cpu = Intcode(program)
    while not cpu.run():
        print("".join(map(chr, cpu.output)))
        cpu.output.clear()
        if steps:
            cmd = steps.pop(0)
        else:
            cmd = input()
        print(cmd)
        cmd += "\n"
        cpu.input.extend(list(map(ord, cmd)))

    print("".join(map(chr, cpu.output)))


with open(sys.argv[1], "r") as f:
    program = list(map(lambda n: int(n), f.readline().split(",")))
    steps = [
        "south",
        "south",
        "south",
        "south",
        "take festive hat",
        "north",
        "north",
        "north",
        "take whirled peas",
        "north",
        "north",
        "take coin",
        "north",
        "north",
        "west",
        "south",
        "west",
        "take mutex",
        "west",
        "south",
        "east"
    ]
    problem1(program, steps)

