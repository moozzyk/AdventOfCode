def run_program(program, noun, verb):
    program[1] = noun
    program[2] = verb
    ip = 0
    while ip < len(program):
        if program[ip] == 1:
            program[program[ip + 3]] = program[program[ip + 1]] + \
                program[program[ip + 2]]
        elif program[ip] == 2:
            program[program[ip + 3]] = program[program[ip + 1]] * \
                program[program[ip + 2]]
        elif program[ip] == 99:
            return program[0]
        ip += 4
    return -1


def problem1(program):
    return run_program(program, 12, 2)


def problem2(program):
    for noun in range(0, 100):
        for verb in range(0, 100):
            result = run_program(program.copy(), noun, verb)
            if result == 19690720:
                return 100 * noun + verb
    return -1


with open("input.txt", "r") as f:
    input = list(map(lambda n: int(n), f.readline().split(",")))
    print(problem1(input.copy()))
    print(problem2(input.copy()))
