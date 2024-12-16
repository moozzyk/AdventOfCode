include("../utils.jl")

DIR = [RIGHT, UP, LEFT, DOWN]
DIR_DEBUG = [ ">", "^", "<", "v"]

function find_cheapest_path(pos, dir_idx, map, visited, points, result, path)
    if map[pos...] == '#'
        return
    end
    if points > result[1][1]
        return
    end
    if map[pos...] == 'E'
        if points < result[1][1]
            pop!(result)
            push!(result, (points, Set(path)))
        else
            union!(result[1][2], Set(path))
        end
        return
    end

    if get(visited, (pos, dir_idx), Inf) < points
        return
    end
    visited[(pos, dir_idx)] = points
    push!(path, pos)
    find_cheapest_path(pos .+ DIR[dir_idx], dir_idx, map, visited, points + 1, result, path)

    dir_left = dir_idx == length(DIR) ? 1 : dir_idx + 1
    if map[(pos .+ DIR[dir_left])...] == '.'
        find_cheapest_path(pos, dir_left, map, visited, points + 1000, result, path)
    end

    dir_right = dir_idx == 1 ? length(DIR) : dir_idx - 1
    if map[(pos .+ DIR[dir_right])...] == '.'
        find_cheapest_path(pos, dir_right, map, visited, points + 1000, result, path)
    end
    pop!(path)
end

function solve(map)
    start_pos = find_in_map('S', map)
    result = Vector{Tuple{Int, Set{Tuple{Int, Int}}}}()
    push!(result, (typemax(Int), Set([])))
    find_cheapest_path(start_pos, 1, map, Dict{Tuple{Tuple{Int, Int}, Int}, Int}(), 0, result, Vector{Tuple{Int, Int}}())
    return result
end

(min_points, spots) = solve(read_map(ARGS[1]))[1]
println(min_points)
println(length(spots) + 1)
