import sys


def problem1(s):
    result = {0: 151, 1: 0, 2: 0}
    for layer in range(100):
        count = {0: 0, 1: 0, 2: 0}
        for pixel in range(150):
            pixel_value = int(s[layer * 150 + pixel])
            count[pixel_value] += 1
        if count[0] < result[0]:
            result = count
    print(result[1] * result[2])


def problem2(s):
    image = ""
    for pixel in range(150):
        for layer in range(100):
            pixel_value = int(s[layer * 150 + pixel])
            if pixel_value != 2:
                image += "*" if pixel_value == 1 else " "
                break
    for start_pos in range(0, 150, 25):
        print(image[start_pos:start_pos + 25])


with open(sys.argv[1], "r") as f:
    line = f.readline()
    problem1(line)
    problem2(line)
