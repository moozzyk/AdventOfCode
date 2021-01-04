import sys


def find_portals(plan):
    portals = {}
    for row in range(len(plan)):
        for col in range(len(plan[row])):
            if plan[row][col] == ".":
                portal_id = ""
                if plan[row - 1][col].isupper():
                    portal_id = plan[row - 2][col] + plan[row - 1][col]
                if plan[row + 1][col].isupper():
                    portal_id = plan[row + 1][col] + plan[row + 2][col]
                if plan[row][col - 1].isupper():
                    portal_id = plan[row][col - 2] + plan[row][col - 1]
                if plan[row][col + 1].isupper():
                    portal_id = plan[row][col + 1] + plan[row][col + 2]

                if portal_id:
                    s = portals.get(portal_id, [])
                    s.append((row, col))
                    portals[portal_id] = s
    return portals


def get_portals(plan):
    portal_list = find_portals(plan)
    start = portal_list.pop("AA", None)[0]
    end = portal_list.pop("ZZ", None)[0]
    portals = {}
    debug = {}
    for portal_name in portal_list:
        (p1, p2) = portal_list[portal_name]
        portals[p1] = p2
        portals[p2] = p1
        debug[p1] = portal_name
        debug[p2] = portal_name
    portals[end] = end
    return (portals, start, end, debug)


def shortest_path_to_portal(start, plan, portals):
    queue = [(start, 0)]
    visited = set()
    exits = []
    while queue:
        (position, steps) = queue.pop(0)
        (row, col) = position
        if position in visited or plan[row][col] != ".":
            continue
        visited.add(position)
        queue.append(((row - 1, col), steps + 1))
        queue.append(((row + 1, col), steps + 1))
        queue.append(((row, col - 1), steps + 1))
        queue.append(((row, col + 1), steps + 1))
        if portals.get((row, col), None) and (row, col) != start:
            exits.append(((row, col), steps))

    return exits


def problem1(plan):
    (portals, start, end, _) = get_portals(plan)

    queue = [(start, 0)]
    visited = set()
    while queue:
        (position, steps) = queue.pop(0)
        if position in visited:
            continue
        visited.add(position)
        if position == end:
            print(steps - 1)  # ZZ is counted as portal
            return
        new_pos = map(lambda p: (portals[p[0]], steps + p[1] + 1),
                      shortest_path_to_portal(position, plan, portals))
        queue.extend(new_pos)


def problem2(plan):
    (portals, start, end, debug) = get_portals(plan)
    min_steps = 10000000
    queue = [(start, 0, 0)]
    visited = {}
    while queue:
        (position, level, steps) = queue.pop(0)
        if visited.get((position, level), 1000000) < steps:
            continue
        if (position == end and level != 0) or steps >= min_steps:
            continue
        visited[(position, level)] = steps
        for p in shortest_path_to_portal(position, plan, portals):
            ((row, col), seg_steps) = p
            if (row, col) == end:
                if level == 0:
                    min_steps = min(min_steps, steps + seg_steps - 1)
                continue
            portal_exit = portals[(row, col)]
            if row == 2 or row == len(plan) - 3 or \
                    col == 2 or col == len(plan[2]) - 3:
                if level != 0:
                    queue.append(
                            (portal_exit, level - 1, steps + seg_steps + 1))
            else:
                queue.append((portal_exit, level + 1, steps + seg_steps + 1))

    print(min_steps + 1)


with open(sys.argv[1], "r") as f:
    plan = list(f.read().splitlines())
    problem1(plan)
    problem2(plan)
