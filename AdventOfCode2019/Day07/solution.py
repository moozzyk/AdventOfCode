import sys
from itertools import permutations


class Amplifier:
    def __init__(self, program, input, output):
        self.program = program
        self.input = input
        self.output = output
        self.ip = 0

    def get_value(self, mode, offset):
        address = self.ip + offset
        if mode == 0:
            return self.program[self.program[address]]
        else:
            return self.program[address]

    def run(self):
        while self.ip < len(self.program):
            opcode = self.program[self.ip] % 100
            modes = self.program[self.ip] // 100
            m1 = modes % 10
            m2 = modes // 10
            if opcode == 1:
                self.program[self.program[self.ip + 3]] = (
                        self.get_value(m1, 1) + self.get_value(m2, 2))
                self.ip += 4
            elif opcode == 2:
                self.program[self.program[self.ip + 3]] = (
                        self.get_value(m1, 1) * self.get_value(m2, 2))
                self.ip += 4
            elif opcode == 3:
                if len(self.input) == 0:
                    return False
                self.program[self.program[self.ip + 1]] = self.input.pop(0)
                self.ip += 2
            elif opcode == 4:
                self.output.append(self.get_value(m1, 1))
                self.ip += 2
            elif opcode == 5:
                if self.get_value(m1, 1) != 0:
                    self.ip = self.get_value(m2, 2)
                else:
                    self.ip += 3
            elif opcode == 6:
                if self.get_value(m1, 1) == 0:
                    self.ip = self.get_value(m2, 2)
                else:
                    self.ip += 3
            elif opcode == 7:
                if self.get_value(m1, 1) < self.get_value(m2, 2):
                    self.program[self.program[self.ip + 3]] = 1
                else:
                    self.program[self.program[self.ip + 3]] = 0
                self.ip += 4
            elif opcode == 8:
                if self.get_value(m1, 1) == self.get_value(m2, 2):
                    self.program[self.program[self.ip + 3]] = 1
                else:
                    self.program[self.program[self.ip + 3]] = 0
                self.ip += 4
            elif opcode == 99:
                return True


def create_amplifiers(program, phases):
    pipes = []
    for s in phases:
        pipes.append([s])
    amplifiers = []
    for i in range(0, len(pipes)):
        in_pipe = pipes[i]
        out_pipe = pipes[(i + 1) % len(pipes)]
        amplifiers.append(Amplifier(list(program), in_pipe, out_pipe))
    return amplifiers


def calculate_thrust(amplifiers):
    current = 0
    amplifiers[0].input.append(0)
    while not amplifiers[current].run() or current != len(amplifiers) - 1:
        current = (current + 1) % len(amplifiers)
    return amplifiers[0].input[0]


def solve(program, phases):
    max_thrust = 0
    for subset in permutations(phases):
        amplifiers = create_amplifiers(program, subset)
        thrust = calculate_thrust(amplifiers)
        max_thrust = max(thrust, max_thrust)
    print(max_thrust)


with open(sys.argv[1], "r") as f:
    program = list(map(lambda n: int(n), f.readline().split(",")))
    solve(program, [0, 1, 2, 3, 4])
    solve(program, [5, 6, 7, 8, 9])
