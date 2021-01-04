import math
import sys


def calculate_ore(resource, required_quantity, graph, leftovers):
    if resource == "ORE":
        return required_quantity

    (num_produced, ingredients) = graph[resource]
    multiplier = math.ceil(required_quantity / num_produced)
    required_ore = 0
    for i in ingredients:
        (n, q) = i
        q *= multiplier
        surplus = leftovers.get(n, 0)
        leftovers[n] = max(surplus - q, 0)
        q = max(q - surplus, 0)
        if q != 0:
            required_ore += calculate_ore(n, q, graph, leftovers)

    new_surplus = num_produced * multiplier - required_quantity
    leftovers[resource] = leftovers.get(resource, 0) + new_surplus
    return required_ore


def create_requirement_graph(lines):
    graph = {}
    for line in lines:
        (left, right) = line.split(" => ")
        (quantity, product) = right.split(" ")
        ingredients = []
        for ingredient in left.split(", "):
            (q, r) = ingredient.split(" ")
            ingredients.append((r, int(q)))
        graph[product] = (int(quantity), ingredients)
    return graph


def problem1(graph):
    print(calculate_ore("FUEL", 1, graph, dict()))


def problem2(graph):
    min_fuel, max_fuel = 1, 10000000
    while min_fuel != max_fuel:
        p = min_fuel + (max_fuel - min_fuel) // 2
        ore_needed = calculate_ore("FUEL", p, graph, dict())
        if ore_needed > 1000000000000:
            max_fuel = p - 1
        else:
            min_fuel = p

    print(max_fuel)


with open(sys.argv[1], "r") as f:
    lines = f.read().splitlines()
    graph = create_requirement_graph(lines)
    problem1(graph)
    problem2(graph)
