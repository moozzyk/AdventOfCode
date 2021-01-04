import sys


def get_number(seq):
    return int("".join(map(str, seq)))


def run_phase(input_seq):
    sums = [0]
    for n in input_seq:
        sums.append(sums[-1] + n)
    out = []
    for digit_idx in range(len(input_seq)):
        multiplier = 1
        group_start = digit_idx
        group_size = digit_idx + 1
        digit = 0
        while group_start < len(input_seq):
            group_end = min(len(input_seq), group_start + group_size)
            partial_sum = sums[group_end] - sums[group_start]
            digit += partial_sum * multiplier
            multiplier *= -1
            group_start += group_size * 2
        out.append(abs(digit) % 10)
    return out


def problem1(input_seq):
    for _ in range(100):
        input_seq = run_phase(input_seq)
    print(get_number(input_seq[:8]))


def problem2(input_seq):
    offset = get_number(input_seq[:7])
    print(offset)
    long_input = input_seq * 10000
    for _ in range(100):
        for idx in range(len(long_input) - 2, offset - 1, -1):
            long_input[idx] = (long_input[idx] + long_input[idx + 1]) % 10
    print(get_number(long_input[offset:offset + 8]))


with open(sys.argv[1], "r") as f:
    line = f.read().splitlines()[0]
    input_seq = list(map(int, line))
    problem1(input_seq)
    problem2(input_seq)
