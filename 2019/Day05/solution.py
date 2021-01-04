def get_value(mode, program, address):
    if mode == 0:
        return program[program[address]]
    else:
        return program[address]


def run_program(program, input):
    ip = 0
    reg = input
    while ip < len(program):
        opcode = program[ip] % 100
        modes = program[ip] // 100
        if opcode == 1:
            program[program[ip + 3]] = get_value(modes % 10, program, ip + 1) \
                    + get_value(modes // 10, program, ip + 2)
            ip += 4
        elif opcode == 2:
            program[program[ip + 3]] = get_value(modes % 10, program, ip + 1) \
                    * get_value(modes // 10, program, ip + 2)
            ip += 4
        elif opcode == 3:
            program[program[ip + 1]] = reg
            ip += 2
        elif opcode == 4:
            reg = get_value(modes % 10, program, ip + 1)
            ip += 2
        elif opcode == 5:
            if get_value(modes % 10, program, ip + 1) != 0:
                ip = get_value(modes // 10, program, ip + 2)
            else:
                ip += 3
        elif opcode == 6:
            if get_value(modes % 10, program, ip + 1) == 0:
                ip = get_value(modes // 10, program, ip + 2)
            else:
                ip += 3
        elif opcode == 7:
            if get_value(modes % 10, program, ip + 1) < \
                    get_value(modes // 10, program, ip + 2):
                program[program[ip + 3]] = 1
            else:
                program[program[ip + 3]] = 0
            ip += 4
        elif opcode == 8:
            if get_value(modes % 10, program, ip + 1) == \
                    get_value(modes // 10, program, ip + 2):
                program[program[ip + 3]] = 1
            else:
                program[program[ip + 3]] = 0
            ip += 4
        elif opcode == 99:
            return reg


with open("input.txt", "r") as f:
    program = list(map(lambda n: int(n), f.readline().split(",")))
    print(run_program(list(program), 1))
    print(run_program(list(program), 5))
