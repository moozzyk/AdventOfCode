import math
import copy


def calculate_energy(values):
    result = []
    for v in values:
        r = 0
        for i in v:
            r += abs(i)
        result.append(r)
    return result


def update_velocity(moons, velocities):
    coordinate = 0
    while coordinate < 3:
        for idx1 in range(len(moons)):
            for idx2 in range(len(moons)):
                if moons[idx1][coordinate] > moons[idx2][coordinate]:
                    velocities[idx1][coordinate] -= 1
                elif moons[idx1][coordinate] < moons[idx2][coordinate]:
                    velocities[idx1][coordinate] += 1
        coordinate += 1

    for m in range(len(moons)):
        for c in range(3):
            moons[m][c] += velocities[m][c]


def problem1(moons):
    velocities = [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]]
    for _ in range(1000):
        update_velocity(moons, velocities)

    pot = calculate_energy(moons)
    kin = calculate_energy(velocities)
    total = 0
    for i in range(len(pot)):
        total += pot[i] * kin[i]
    print(total)


def lcm(n1, n2):
    return (n1 * n2)//math.gcd(n1, n2)


def problem2(moons):
    initial = copy.deepcopy(moons)
    velocities = [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]]
    zero_points = [[set(), set(), set()] for _ in moons]
    periods = [[0] * 3 for _ in velocities]
    step = 0
    while any(0 in l for l in periods):
        for m in range(len(velocities)):
            for c in range(len(velocities[m])):
                if velocities[m][c] != 0 or moons[m][c] != initial[m][c]:
                    continue
                if periods[m][c]:
                    continue
                if step % 2 == 0 and step // 2 in zero_points[m][c]:
                    periods[m][c] = step // 2
                zero_points[m][c].add(step)
        update_velocity(moons, velocities)
        step += 1

    result = 1
    for p in periods:
        moon_result = 1
        for n in p:
            moon_result = lcm(moon_result, n)
        result = lcm(result, moon_result)

    print(result)


moons = [[17, -7, -11], [1, 4, -1], [6, -2, -6], [19, 11, 9]]
problem1(copy.deepcopy(moons))
problem2(copy.deepcopy(moons))
