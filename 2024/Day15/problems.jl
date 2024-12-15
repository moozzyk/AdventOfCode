include("../utils.jl")

function read_input(file_name)
    lines = readlines(file_name)
    map = read_map(filter(l -> occursin("#", l), lines))
    moves = string(filter(l -> occursin("<", l), lines)...)
    return (map, moves)
end

function find_robot(map)
    return filter(p -> map[p...] == '@', Tuple.(CartesianIndices(map)))[1]
end

function find_empty_spot(pos, dir, map)
    while true
        if (map[pos...] == '.')
            return pos
        end
        if (map[pos...] == '#')
            return nothing
        end
        pos = pos .+ dir
    end
end

function move(pos, target, dir, map)
    while true
        src = target .- dir
        map[target...] = map[src...]
        if src == pos
            map[src...] = '.'
            break
        end
        target = src
    end
end

function move_robot(pos, map, moves)
    dirs = Dict('^'=>UP, 'v'=>DOWN, '<'=>LEFT, '>'=>RIGHT)
    for m in moves
        dir = dirs[m]
        empty_spot = find_empty_spot(pos, dir, map)
        if empty_spot != nothing
            move(pos, empty_spot, dir, map)
            pos = pos .+ dir
        end
    end
end

function compute_distance(pos, map)
    if map[pos...] == 'O'
        return (pos[1] - 1) * 100 + pos[2] - 1
    end
    return 0
end

function problem1(map, moves)
    robot_pos = find_robot(map)
    move_robot(robot_pos, map, moves)
    result = 0
    for pos in Tuple.(CartesianIndices(map))
        result += compute_distance(pos, map)
    end
    return result
end

(map, moves) = read_input(ARGS[1])
println(problem1(map, moves))