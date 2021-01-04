def num_counts(n):
    counts = dict()
    while n > 0:
        r = n % 10
        counts[r] = counts.get(r, 0) + 1
        n //= 10
    return counts


def is_growing(n):
    digit = 9
    while n > 0:
        current_digit = n % 10
        if current_digit > digit:
            return False
        digit = current_digit
        n //= 10
    return True


def solve(min, max, is_valid):
    count = 0
    for n in range(min, max + 1):
        if is_growing(n) and is_valid(num_counts(n)):
            count += 1
    return count


print(solve(137683, 596253,
            lambda counts: any(i > 1 for i in counts.values())))
print(solve(137683, 596253, lambda counts: 2 in counts.values()))
