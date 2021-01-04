import sys
from intcode import Intcode


def problem1(program):
    input, output = [], []
    cpu = Intcode(program, input, output)
    cpu.run()
    block_tiles = 0
    for i in range(0, len(output), 3):
        if output[i + 2] == 2:
            block_tiles += 1
    print(block_tiles)


def count_blocks(screen):
    num_blocks = 0
    for row in screen:
        num_blocks += row.count("o")
    return num_blocks


def problem2(program):
    tiles = [" ", "#", "o", "^", "*"]
    screen = [['.'] * 45 for i in range(26)]
    input, output = [], []
    program[0] = 2
    cpu = Intcode(program, input, output)

    bat_pos, ball_pos, score = 0, 0, 0
    blocks_remaining = 1
    while blocks_remaining > 0:
        cpu.run()
        while output:
            col, row = output.pop(0), output.pop(0)
            if col == -1 and row == 0:
                score = output.pop(0)
            else:
                tile_type = tiles[output.pop(0)]
                screen[row][col] = tile_type
                if tile_type == "*":
                    ball_pos = col
                if tile_type == "^":
                    bat_pos = col
        # could just track blocks based on whether
        # a block is being removed
        blocks_remaining = count_blocks(screen)
        if ball_pos > bat_pos:
            input.append(1)
        elif ball_pos < bat_pos:
            input.append(-1)
        else:
            input.append(0)

        for l in screen:
            print("".join(l))
        print(score)


with open(sys.argv[1], "r") as f:
    program = list(map(lambda n: int(n), f.readline().split(",")))
    problem1(program)
    problem2(program)
