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

function move(pos, dir, map)
    new_pos = pos .+ dir
    if map[new_pos...] == '#'
        return false
    end
    if map[new_pos...] == 'O'
        if !move(new_pos, dir, map)
            return false
        end
    end
    map[new_pos...] = map[pos...]
    map[pos...] = '.'
    return true
end

function move_robot(pos, map, moves)
    dirs = Dict('^'=>UP, 'v'=>DOWN, '<'=>LEFT, '>'=>RIGHT)
    for m in moves
        dir = dirs[m]
        if move(pos, dir, map)
            pos = pos .+ dir
        end
    end
end

function compute_gps(map)
    return sum(t -> t[1] * 100 + t[2] - 101, filter(p -> map[p...] == 'O', Tuple.(CartesianIndices(map))))
end

function problem1(map, moves)
    robot_pos = find_robot(map)
    move_robot(robot_pos, map, moves)
    return compute_gps(map)
end

(map, moves) = read_input(ARGS[1])
println(problem1(map, moves))