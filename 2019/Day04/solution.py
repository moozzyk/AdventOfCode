def has_consecutive(number):
    i = 1
    while i < len(number):
        if number[i] == number[i - 1]:
            return True
        i += 1
    return False


def has_2_consecutive(number):
    i = 0
    while i < len(number) - 1:
        num_consecutive = 0
        while i + num_consecutive < len(number) and number[i] == number[
                i + num_consecutive]:
            num_consecutive += 1

        if num_consecutive == 2:
            return True

        i += num_consecutive

    return False


def recurse(min, max, number, digit, req_fn):
    if (len(number) > len(max)):
        return 0

    count = 0
    if len(number) > 0 and number[0] != '0' and int(number) >= int(
            min) and int(number) <= int(max) and req_fn(number):
        count += 1

    for d in range(digit, 10):
        count += recurse(min, max, number + str(d), d, req_fn)

    return count


def problem1(min, max):
    return recurse(str(min), str(max), "", 0, has_consecutive)


def problem2(min, max):
    return recurse(str(min), str(max), "", 0, has_2_consecutive)


print(problem1(137683, 596253))
print(problem2(137683, 596253))
