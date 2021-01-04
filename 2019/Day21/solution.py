import sys
from intcode import Intcode


def execute(program, script):
    cpu = Intcode(program, [], [])
    for line in script:
        cpu.input.extend(list(map(ord, line + "\n")))

    cpu.run()
    if cpu.output[-1] < 128:
        print("".join(map(chr, cpu.output)))
    else:
        print(cpu.output[-1])


def problem1(program):
    script = [
            "NOT A T",
            "NOT C J",
            "AND D J",
            "OR T J",
            "WALK",
            ]
    execute(program, script)


def problem2(program):
    script = [
            "NOT A J",
            "NOT J J",
            "AND B J",
            "AND C J",
            "NOT J J",
            "AND D J",

            "NOT E T",
            "NOT T T",
            "AND I T",
            "OR H T",
            "AND T J",

            "NOT A T",
            "OR T J",

            "RUN",
            ]
    execute(program, script)


with open(sys.argv[1], "r") as f:
    program = list(map(lambda n: int(n), f.readline().split(",")))
    problem1(program)
    problem2(program)
