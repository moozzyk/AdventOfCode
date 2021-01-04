import sys
from intcode import Intcode

def build_network(program):
    return [Intcode(program, input_queue=[i, -1], output_queue=[]) for i in range(50)]


def is_idle(network):
    for cpu in network:
        if cpu.input:
            return False
    return True


def run_network(network, nat=(None, None, None)):
    (nat_x, nat_y, _) = nat
    while True:
        for i in range(len(network)):
            cpu = network[i]
            cpu.run()
            if not cpu.output and is_idle(network):
                network[0].input.extend([nat_x, nat_y])
                return (None, None, nat_y)
            while cpu.output:
                address = cpu.output.pop(0)
                x = cpu.output.pop(0)
                y = cpu.output.pop(0)
                if address == 255:
                   return (x, y, None)
                else:
                    network[address].input.extend([x, y])


def problem1(program):
    (_, y, _) = run_network(build_network(program))
    print(y)


def problem2(program):
    nat = (None, None, None)
    network = build_network(program)
    while True:
        (x, y, last_y) = run_network(network, nat)
        if last_y and last_y == nat[2]:
            print(last_y)
            return
        nat = (x or nat[0], y or nat[1], last_y or nat[2])


with open(sys.argv[1], "r") as f:
    program = list(map(lambda n: int(n), f.readline().split(",")))
    problem1(program)
    problem2(program)