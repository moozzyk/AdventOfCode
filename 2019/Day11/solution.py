import sys


class Intcode:
    def __init__(self, program, input, output):
        self.memory = {i: program[i] for i in range(len(program))}
        self.input = input
        self.output = output
        self.ip = 0
        self.base = 0

    def get_value_at(self, address):
        return self.memory.get(address, 0)

    def get_value(self, mode, offset):
        address = self.ip + offset
        if mode == 0:
            return self.get_value_at(self.get_value_at(address))
        if mode == 2:
            return self.get_value_at(self.base + self.get_value_at(address))
        return self.get_value_at(address)

    def set_value(self, mode, offset, value):
        address = self.get_value_at(self.ip + offset)
        if mode == 2:
            address = self.base + address
        self.memory[address] = value

    def run(self):
        while True:
            opcode = self.get_value_at(self.ip) % 100
            modes = self.get_value_at(self.ip) // 100
            m1 = modes % 10
            modes = modes // 10
            m2 = modes % 10
            modes = modes // 10
            m3 = modes % 10
            if opcode == 1:
                value = self.get_value(m1, 1) + self.get_value(m2, 2)
                self.set_value(m3, 3, value)
                self.ip += 4
            elif opcode == 2:
                value = self.get_value(m1, 1) * self.get_value(m2, 2)
                self.set_value(m3, 3, value)
                self.ip += 4
            elif opcode == 3:
                if len(self.input) == 0:
                    return False
                self.set_value(m1, 1, self.input.pop(0))
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
                    self.set_value(m3, 3, 1)
                else:
                    self.set_value(m3, 3, 0)
                self.ip += 4
            elif opcode == 8:
                if self.get_value(m1, 1) == self.get_value(m2, 2):
                    self.set_value(m3, 3, 1)
                else:
                    self.set_value(m3, 3, 0)
                self.ip += 4
            elif opcode == 9:
                self.base += self.get_value(m1, 1)
                self.ip += 2
            elif opcode == 99:
                return True


#  direction
#  - 0 - ^
#  - 1 - >
#  - 2 - v
#  - 3 - <
def paint(program, color):
    turns = [-1, 1]
    visited = dict()
    x, y, direction = 0, 0, 0
    input, output = [color], []
    cpu = Intcode(program, input, output)
    while True:
        if not input:
            input.append(visited.get((x, y), 0))
        if cpu.run():
            break
        last_color = output.pop(0)
        turn = output.pop(0)
        direction = (direction + turns[turn]) % 4
        visited[(x, y)] = last_color
        if direction == 0:
            y -= 1
        elif direction == 1:
            x += 1
        elif direction == 2:
            y += 1
        elif direction == 3:
            x -= 1
    return visited


def draw(painted):
    screen = [[" "] * 43 for i in range(6)]
    for (x, y), color in painted.items():
        screen[y][x] = ' ' if color == 0 else '#'

    for l in screen:
        print("".join(l))


def problem1(program):
    painted = paint(program, 0)
    print(len(painted))


def problem2(program):
    painted = paint(program, 1)
    draw(painted)


with open(sys.argv[1], "r") as f:
    program = list(map(lambda n: int(n), f.readline().split(",")))
    problem1(program)
    problem2(program)
