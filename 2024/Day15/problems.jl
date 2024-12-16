include("../utils.jl")

function read_input(file_name)
    lines = readlines(file_name)
    map = filter(l -> occursin("#", l), lines)
    moves = string(filter(l -> occursin("^", l), lines)...)
    return (map, moves)
end

function find_robot(map)
    return filter(p -> map[p...] == '@', Tuple.(CartesianIndices(map)))[1]
end

function compute_gps(map)
    return sum(t -> t[1] * 100 + t[2] - 101, filter(p -> map[p...] in Set(['O', '[']), Tuple.(CartesianIndices(map))))
end

function can_move(pos, dir, map)
    new_pos = pos .+ dir
    if map[new_pos...] == '#'
        return false
    elseif map[new_pos...] == '.'
        return true
    elseif map[new_pos...] == 'O'
        return can_move(new_pos, dir, map)
    else
        if dir == LEFT || dir == RIGHT
            return can_move(new_pos, dir, map)
        else
            if map[new_pos...] == '['
                return can_move(new_pos, dir, map) && can_move(new_pos .+ (0, 1), dir, map)
            elseif map[new_pos...] == ']'
                return can_move(new_pos, dir, map) && can_move(new_pos .- (0, 1), dir, map)
            else
                throw("Unexpected condition")
            end
        end
    end
end

function move(pos, dir, map)
    # box already moved
    if map[pos...] == '.'
        return
    end
    new_pos = pos .+ dir
    if map[new_pos...] == 'O'
        move(new_pos, dir, map)
    elseif map[new_pos...] == '['
        move(new_pos, dir, map)
        if dir == UP || dir == DOWN
            move(new_pos .+ (0, 1), dir, map)
            map[(new_pos .+ (0, 1))...] = map[(pos .+ (0, 1))...]
            map[(new_pos .+ (0, 1))...] = '.'
        end
    elseif map[new_pos...] == ']'
        move(new_pos, dir, map)
        if dir == UP || dir == DOWN
            move(new_pos .- (0, 1), dir, map)
            map[(new_pos .- (0, 1))...] = map[(pos .+ (0, 1))...]
            map[(new_pos .- (0, 1))...] = '.'
        end
    end
    map[new_pos...] = map[pos...]
    map[pos...] = '.'
end

function move_robot(map, moves)
    pos = find_robot(map)
    dirs = Dict('^'=>UP, 'v'=>DOWN, '<'=>LEFT, '>'=>RIGHT)
    for m in moves
        dir = dirs[m]
        if can_move(pos, dir, map)
            move(pos, dir, map)
            pos = pos .+ dir
        end
    end
end

function solve(lines, moves)
    map = lines_to_char_map(lines)
    move_robot(map, moves)
    return compute_gps(map)
end

function translate_char(c)
    if c == '@'
        return "@."
    elseif c == '.'
        return ".."
    elseif c == '#'
        return "##"
    elseif c == 'O'
        return "[]"
    end
end

scale_row(row) = string(translate_char.(collect(row))...)

problem1(lines, move) = solve(lines, moves)
problem2(lines, moves) = solve(scale_row.(lines), moves)

(lines, moves) = read_input(ARGS[1])
println(problem1(lines, moves))
println(problem2(lines, moves))
