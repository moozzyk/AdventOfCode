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
    visited = Set([pos])
    while true
        (next_row, next_col) = pos .+ directions[curr_dir + 1]
        if next_row < 1 || next_row > length(map) || next_col < 1 || next_col > length(map[next_row])
            return visited
        end
        if map[next_row][next_col] == '#'
            curr_dir = (curr_dir + 1) % 4
        else
            pos = (next_row, next_col)
            push!(visited, pos)
        end
    end
end

function problem1(map)
    start_pos = find_start_pos(map)
    return walk(map, start_pos) |> length
end

map = readlines(ARGS[1]) |> lines -> [collect(l) for l in lines]
println(problem1(map))