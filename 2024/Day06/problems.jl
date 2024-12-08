function find_start_pos(map)
    for row in eachindex(map)
        for col in eachindex(map[row])
            if map[row][col] == '^'
                return (row, col)
            end
        end
    end
end

function walk(map, pos)
    directions = [(-1, 0), (0, 1), (1, 0), (0, -1)]
    curr_dir = 0
    steps = 0
    visited = Set([pos])
    turns = Set{Tuple{Int, Int, Int}}()
    while true
        (next_row, next_col) = pos .+ directions[curr_dir + 1]
        if next_row < 1 || next_row > length(map) || next_col < 1 || next_col > length(map[next_row])
            return visited
        end
        if map[next_row][next_col] == '#'
            turn = (row, col, curr_dir)
            if (turn in turns)
                return nothing
            end
            push!(turns, turn)
            curr_dir = (curr_dir + 1) % 4
        else
            pos = (next_row, next_col)
            if pos in visited
                steps += 1
            else
                push!(visited, pos)
                steps = 0
            end
            # dumb cycle detection
            if steps > 6000
                return nothing
            end
        end
    end
end

function problem1(map)
    start_pos = find_start_pos(map)
    return walk(map, start_pos) |> length
end

function problem2(map)
    start_pos = find_start_pos(map)
    visited = walk(map, start_pos)
    num_cycles = 0
    for pos in visited
        if pos == start_pos
            continue
        end
        map[pos[1]][pos[2]] = '#'
        if isnothing(walk(map, start_pos))
            num_cycles += 1
        end
        map[pos[1]][pos[2]] = '.'
    end
    return num_cycles
end

map = readlines(ARGS[1]) |> lines -> [collect(l) for l in lines]
println(problem1(map))
println(problem2(map))