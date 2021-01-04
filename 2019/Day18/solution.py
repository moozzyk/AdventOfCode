import sys


def find_keys(maze):
    keys = {}
    for row in range(len(maze)):
        for col in range(len(maze[row])):
            tile = maze[row][col]
            if tile == "@" or (tile.isalpha() and tile.islower()):
                keys[tile] = (row, col)
    return keys


def get_key_vector(keys):
    all_keys = 0
    for k in keys:
        if not(k.isalpha() and k.islower()):
            assert("Incorrect key value")
        all_keys |= 1 << (ord(k) - ord("a"))
    return all_keys


def get_path_to_key(start, end_key, maze):
    q = [(start, 0, 0, 0)]
    visited = set()
    while q:
        ((row, col), steps, required_keys, collected_keys) = q.pop(0)
        if row < 0 or col < 0 or row >= len(maze) or col >= len(maze[row]) \
                or maze[row][col] == "#" or (row, col) in visited:
            continue
        visited.add((row, col))

        tile = maze[row][col]
        if tile.isalpha():
            key = 1 << (ord(tile.lower()) - ord("a"))
            if tile.islower():
                collected_keys |= key
            else:
                if collected_keys & key == 0:
                    required_keys |= key

        if tile == end_key:
            return ((row, col), steps, required_keys, collected_keys)

        for (d_row, d_col) in [(-1, 0), (1, 0), (0, 1), (0, -1)]:
            q.append(((row + d_row, col + d_col), steps + 1, required_keys,
                     collected_keys))


def get_path_to_key_cached(start, end_key, maze, paths):
    start_key = maze[start[0]][start[1]]
    path_points = (start_key, end_key)
    if path_points not in paths.keys():
        paths[path_points] = get_path_to_key(start, end_key, maze)
    return paths[path_points]


def get_path(start, all_keys, maze):
    cached_paths = {}
    visited = {}
    q = [(start, 0, 0)]
    min_steps = 10000000
    while q:
        (pos, steps, collected_keys) = q.pop(0)
        if collected_keys == all_keys:
            min_steps = min(min_steps, steps)
            continue
        num_steps = visited.get((pos, collected_keys), 10000000)
        if steps >= num_steps:
            continue
        visited[(pos, collected_keys)] = steps

        for key in range(bin(all_keys).count("1")):
            if collected_keys & (1 << key) != 0:
                continue
            target_key = chr(ord("a") + key)
            (new_pos, seg_steps, req_keys, new_keys) = get_path_to_key_cached(
                    pos, target_key, maze, cached_paths)
            if collected_keys & req_keys == req_keys:
                q.append((new_pos, steps + seg_steps,
                         collected_keys | new_keys))

    return min_steps


def problem1(maze):
    keys = find_keys(maze)
    start = keys.pop("@")
    all_keys = get_key_vector(keys)
    min_steps = get_path(start, all_keys, maze)
    print(min_steps)


def update_maze(maze):
    keys = find_keys(maze)
    (row, col) = keys.pop("@")
    for i in range(-1, 2):
        maze[row + i] = maze[row + i][:col] + "#" + maze[row + i][col + 1:]
        maze[row] = maze[row][:col + i] + "#" + maze[row][col + i + 1:]

    return (row, col)


def split_maze(maze, split_point):
    mazes = [[] for _ in range(4)]

    for row in range(split_point[0] + 1):
        mazes[0].append(maze[row][:split_point[1] + 1])
        mazes[1].append(maze[row][split_point[1]:])

    for row in range(split_point[0], len(maze)):
        mazes[2].append(maze[row][:split_point[1] + 1])
        mazes[3].append(maze[row][split_point[1]:])

    (row, col) = split_point
    start_points = [(row - 1, col - 1), (row - 1, 1), (1, col - 1), (1, 1)]
    maze_data = []
    for i in range(len(mazes)):
        maze_data.append((mazes[i], start_points[i],
                         get_key_vector(find_keys(mazes[i]))))
    return maze_data


def get_keys(mazes, all_keys):
    cached_paths = {}
    visited = {}
    min_steps = 1000000
    start = []
    for m in mazes:
        start.append((m[1], 0))
    q = [(tuple(start), 0, 0)]
    while q:
        (robots), steps, collected_keys = q.pop(0)
        if collected_keys == all_keys:
            min_steps = min(min_steps, steps)
            continue
        num_steps = visited.get(((robots), collected_keys), 1000000)
        if steps >= num_steps:
            continue
        visited[((robots), collected_keys)] = steps
        for i in range(len(robots)):
            (pos, robot_keys) = robots[i]
            maze_keys = mazes[i][2]
            key = 0
            while maze_keys > 0:
                if maze_keys & 1 != 0 and robot_keys & (1 << key) == 0:
                    target_key = chr(ord("a") + key)

                    path_details = get_path_to_key_cached(
                            pos, target_key, mazes[i][0], cached_paths)
                    (new_pos, seg_steps, req_keys, new_keys) = path_details
                    if collected_keys & req_keys == req_keys:
                        new_state = []
                        new_state.extend(robots[:i])
                        new_state.append(((new_pos), robot_keys | new_keys))
                        new_state.extend(robots[i + 1:])

                        q.append((tuple(new_state), steps + seg_steps,
                                 collected_keys | new_keys))
                key += 1
                maze_keys //= 2
    return min_steps


def problem2(maze):
    keys = find_keys(maze)
    split_point = keys.pop("@")
    all_keys = get_key_vector(keys)
    update_maze(maze)
    mazes = split_maze(maze, split_point)
    min_steps = get_keys(mazes, all_keys)
    print(min_steps)


with open(sys.argv[1], "r") as f:
    maze = f.read().splitlines()
    problem1(maze)
    problem2(maze)
