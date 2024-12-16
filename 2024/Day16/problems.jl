include("../utils.jl")

DIR = [RIGHT, UP, LEFT, DOWN]
DIR_DEBUG = [ ">", "^", "<", "v"]

function find_cheapest_path(pos, dir_idx, map, visited, points, results)
    if map[pos...] == '#'
        return
    end
    if length(results) > 0 && points >= minimum(results)
        return
    end
    if map[pos...] == 'E'
        push!(results, points)
        return
    end

    if get(visited, (pos, dir_idx), Inf) < points
        return
    end
    visited[(pos, dir_idx)] = points
    find_cheapest_path(pos .+ DIR[dir_idx], dir_idx, map, visited, points + 1, results)

    dir_left = dir_idx == length(DIR) ? 1 : dir_idx + 1
    if map[(pos .+ DIR[dir_left])...] == '.'
        find_cheapest_path(pos, dir_left, map, visited, points + 1000, results)
    end

    dir_right = dir_idx == 1 ? length(DIR) : dir_idx - 1
    if map[(pos .+ DIR[dir_right])...] == '.'
        find_cheapest_path(pos, dir_right, map, visited, points + 1000, results)
    end
end

function problem1(map)
    start_pos = find_in_map('S', map)
    results = Vector{Int}()
    find_cheapest_path(start_pos, 1, map, Dict{Tuple{Tuple{Int, Int}, Int}, Int}(), 0, results)
    return results |> minimum
end

map = read_map(ARGS[1])
println(problem1(map))