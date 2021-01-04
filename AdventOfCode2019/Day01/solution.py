def get_fuel_requirement(mass):
    return int(mass/3) - 2


def problem1(lines):
    sum = 0
    for line in lines: 
        sum += get_fuel_requirement(int(line)) 
    return sum


def problem2(lines):
    result = 0
    for line in lines:
        mass = int(line)
        while mass > 6:
            fuel_requirement = get_fuel_requirement(mass)
            result += fuel_requirement
            mass = fuel_requirement
    return result


with open("input.txt", "r") as f:
    lines = list(f)
    print(problem1(lines))
    print(problem2(lines))


