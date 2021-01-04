import sys


def build_orbits(lines):
    orbits = {}
    for l in lines:
        parts = l.split(")")
        orbits[parts[1]] = parts[0]
    return orbits


def get_path(start, obrbits):
    path = []
    while start != "COM":
        path.append(start)
        start = orbits[start]
    return path


def problem1(orbits):
    total_orbits = 0
    for k in orbits.keys():
        total_orbits += len(get_path(k, orbits))
    print(total_orbits)


def problem2(orbits):
    p1 = get_path("YOU", orbits)
    p2 = get_path("SAN", orbits)
    i, j = len(p1) - 1, len(p2) - 1
    while p1[i] == p2[j]:
        i -= 1
        j -= 1
    print(i + j)


with open(sys.argv[1], "r") as f:
    lines = f.read().splitlines()
    orbits = build_orbits(lines)
    problem1(orbits)
    problem2(orbits)
