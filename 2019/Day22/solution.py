import sys


def problem1(operations, deck_size):
    deck = list(range(deck_size))
    for op in operations:
        if op.startswith("cut"):
            n = int(op[4:])
            deck = deck[n:] + deck[:n]
        elif op.startswith("deal with"):
            n = int(op[20:])
            new_deck = [0] * deck_size
            target_pos = 0
            for i in range(deck_size):
                new_deck[target_pos] = deck[i]
                target_pos = (target_pos + n) % deck_size
            deck = new_deck
        elif op.startswith("deal into"):
            deck = deck[::-1]
            None
        else:
            assert("Unrecognized operation.")
    print(deck.index(2019))


def shuffle_step(operation, card, deck_size):
    if operation.startswith("cut"):
        n = int(operation[4:])
        return (card + deck_size + n) % deck_size
    elif operation.startswith("deal with"):
        n = int(operation[20:])
        return (card * pow(n, deck_size - 2, deck_size)) % deck_size
    elif operation.startswith("deal into"):
        return deck_size - 1 - card
    else:
        assert("Unrecognized operation.")


def shuffle(operations, card, deck_size):
    for operation in reversed(operations):
        card = shuffle_step(operation, card, deck_size)
    return card


def problem2(operations, card, deck_size, reps):
    b = shuffle(operations, 0, deck_size)
    a = shuffle(operations, 1, deck_size) - b
    # f^n(x) = a^n*x + b*(a^n - 1)/(a - 1)
    res = (pow(a, reps, deck_size) * card + b * (pow(a, reps, deck_size) - 1)
           * pow(a - 1, -1, deck_size)) % deck_size
    print(res)


with open(sys.argv[1], "r") as f:
    lines = f.read().splitlines()
    problem1(lines, 10007)
    problem2(lines, 2020, 119315717514047, 101741582076661)
