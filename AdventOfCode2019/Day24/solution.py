import sys


def next_turn(bugs, row, col):
    num_bugs = 0
    num_bugs += 1 if row > 0 and bugs[row - 1][col] == "#" else 0
    num_bugs += 1 if row < len(bugs) - 1 and bugs[row + 1][col] == "#" else 0
    num_bugs += 1 if col > 0 and bugs[row][col - 1] == "#" else 0
    num_bugs += 1 if col < len(bugs[row]) - \
        1 and bugs[row][col + 1] == "#" else 0

    if bugs[row][col] == "#" and num_bugs != 1:
        return "."
    if bugs[row][col] == "." and (num_bugs == 1 or num_bugs == 2):
        return "#"
    return bugs[row][col]


def transform(bugs):
    new_bugs = [[] for i in range(len(bugs))]
    for row in range(len(bugs)):
        for col in range(len(bugs[row])):
            new_bugs[row].append(next_turn(bugs, row, col))
    return new_bugs


def calculate_biodiversity(bugs):
    power = 1
    biodiversity = 0
    for row in bugs:
        for field in row:
            biodiversity += power if field == "#" else 0
            power *= 2
    return biodiversity


def problem1(bugs):
    seen = set([calculate_biodiversity(bugs)])

    while True:
        bugs = transform(bugs)
        biodiversity = calculate_biodiversity(bugs)
        if biodiversity in seen:
            print(biodiversity)
            return
        seen.add(biodiversity)


class BugLevel:
    def __init__(self, outer, inner):
        self.outer = outer
        self.inner = inner
        self.bugs = [[0] * 5 for _ in range(len(bugs))]

    def turn(self, processed):
        if self in processed:
            return
        processed.add(self)
        new_bugs = [[0] * 5 for _ in range(len(bugs))]
        self.calculate_outer_layer(new_bugs)
        self.calculate_inner_layer(new_bugs)
        bug_count = self.num_bugs(new_bugs)
        if bug_count > 0 or self.inner:
            if not self.inner:
                self.inner = BugLevel(self, None)
            self.inner.turn(processed)
        if bug_count > 0 or self.outer:
            if not self.outer:
                self.outer = BugLevel(None, self)
            self.outer.turn(processed)
        self.bugs = new_bugs

    def calculate_outer_layer(self, new_bugs):
        bugs_outer_up = 0
        bugs_outer_down = 0
        bugs_outer_left = 0
        bugs_outer_right = 0
        if self.outer:
            bugs_outer_up = self.outer.bugs[1][2]
            bugs_outer_down = self.outer.bugs[3][2]
            bugs_outer_left = self.outer.bugs[2][1]
            bugs_outer_right = self.outer.bugs[2][3]

        new_bugs[0][0] = self.calculate(
            self.bugs[0][0],
            bugs_outer_left + bugs_outer_up + self.bugs[0][1] + self.bugs[1][0])
        new_bugs[0][4] = self.calculate(
            self.bugs[0][4],
            self.bugs[0][3] + bugs_outer_up + bugs_outer_right + self.bugs[1][4])
        new_bugs[4][0] = self.calculate(
            self.bugs[4][0],
            bugs_outer_left + self.bugs[3][0] + self.bugs[4][1] + bugs_outer_down)
        new_bugs[4][4] = self.calculate(
            self.bugs[4][4],
            self.bugs[4][3] + self.bugs[3][4] + bugs_outer_right + bugs_outer_down)

        for i in range(1, 4):
            new_bugs[0][i] = self.calculate(
                self.bugs[0][i],
                self.bugs[0][i - 1] + bugs_outer_up + self.bugs[0][i + 1] + self.bugs[1][i])
            new_bugs[4][i] = self.calculate(
                self.bugs[4][i],
                self.bugs[4][i - 1] + bugs_outer_down + self.bugs[4][i + 1] + self.bugs[3][i])
            new_bugs[i][0] = self.calculate(
                self.bugs[i][0],
                bugs_outer_left + self.bugs[i - 1][0] + self.bugs[i][1] + self.bugs[i + 1][0])
            new_bugs[i][4] = self.calculate(
                self.bugs[i][4],
                self.bugs[i][3] + self.bugs[i - 1][4] + bugs_outer_right + self.bugs[i + 1][4])

    def calculate_inner_layer(self, new_bugs):
        bugs_inner_up = 0
        bugs_inner_down = 0
        bugs_inner_left = 0
        bugs_inner_right = 0
        if self.inner:
            for i in range(5):
                bugs_inner_up += self.inner.bugs[0][i]
                bugs_inner_down += self.inner.bugs[4][i]
                bugs_inner_left += self.inner.bugs[i][0]
                bugs_inner_right += self.inner.bugs[i][4]

        for row in range(1, 4):
            for col in range(1, 4):
                if row == 2 and col == 2:
                    continue
                if row == 1:
                    self.bugs[2][2] = bugs_inner_up
                if row == 2:
                    self.bugs[2][2] = bugs_inner_left if col == 1 else bugs_inner_right
                if row == 3:
                    self.bugs[2][2] = bugs_inner_down
                new_bugs[row][col] = self.calculate(
                    self.bugs[row][col],
                    self.bugs[row][col - 1] + self.bugs[row - 1][col] + self.bugs[row][col + 1] + self.bugs[row + 1][col])
            self.bugs[2][2] = 0

    def calculate(self, tile, num_neighbor_bugs):
        if tile == 1 and num_neighbor_bugs != 1:
            return 0
        if tile == 0 and (num_neighbor_bugs == 1 or num_neighbor_bugs == 2):
            return 1
        return tile

    def bugs_inner_up(self):
        return 0 if not self.outer else self.outer[4].count("#")

    def bugs_inner_down(self):
        return 0 if not self.outer else self.outer[0].count("#")

    def num_bugs(self, bugs):
        total = 0
        for row in range(len(bugs)):
            for col in range(len(bugs[row])):
                if row == 2 and col == 2:
                    continue
                total += bugs[row][col]
        return total

    def print(self):
        for row in range(len(self.bugs)):
            for col in range(len(self.bugs[row])):
                tile = "#" if self.bugs[row][col] == 1 else "."
                if row == 2 and col == 2:
                    tile = "?"
                print(tile, end="")
            print()


def print_levels(level):
    while level.outer:
        level = level.outer

    while level:
        level.print()
        print()
        level = level.inner


def count_bugs(level):
    while level.outer:
        level = level.outer

    num_bugs = 0
    while level:
        num_bugs += level.num_bugs(level.bugs)
        level = level.inner
    return num_bugs


def problem2(bugs):
    translated_bugs = []
    for l in bugs:
        translated_bugs.append(list(map(lambda b: 1 if b == "#" else 0, l)))
    base_level = BugLevel(None, None)
    base_level.bugs = translated_bugs

    for _ in range(200):
        base_level.turn(set())
    print(count_bugs(base_level))

with open(sys.argv[1], "r") as f:
    bugs = f.read().splitlines()
    problem1(bugs)
    problem2(bugs)
