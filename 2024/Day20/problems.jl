# import Pkg; Pkg.add("DataStructures")
using DataStructures

include("../utils.jl")

function find_path(start_pos, end_pos, map, visited = Set{Tuple{Int, Int}}())
    q = Queue{Tuple{Tuple{Int, Int}, Int}}()
    distances = Dict{Tuple{Int, Int}, Int}()
    enqueue!(q, (start_pos, 0))
    while length(q) > 0
        (pos, steps) = dequeue!(q)
        distances[pos] = steps
        if pos == end_pos
            return distances
        end
        push!(visited, pos)
        for dir in DIRECTIONS
            new_pos = pos .+ dir
            if !(safe_get(map, new_pos, '#') == '#' || new_pos in visited)
                enqueue!(q, (new_pos, steps + 1))
            end
        end
    end
    return nothing
end

function solve(map, radius)
    start_pos = find_in_map('S', map)
    end_pos = find_in_map('E', map)
    path = Set{Tuple{Int, Int}}()
    distances = find_path(end_pos, start_pos, map, path)
    normal_path_length = distances[start_pos]
    result = Vector{Tuple{Int, Tuple{Int, Int}, Tuple{Int, Int}}}()

    for (pos, distance) in distances
        for row in -radius:radius
            for col in -radius:radius
                hack_end = pos .+ (row, col)
                if abs(row) + abs(col) > radius || safe_get(map, hack_end, '#') == '#'
                    continue
                end
                steps_to_end = get(distances, hack_end, Inf)
                if steps_to_end != Inf
                    steps = normal_path_length - distances[pos] + steps_to_end + abs(row) + abs(col)
                    if steps < normal_path_length
                        push!(result, (normal_path_length - steps, pos, pos .+ (row, col)))
                    end
                end
            end
        end
    end
    return length(filter((l) -> l[1] >= 100, result))
end

problem1(map) = solve(map, 2)
problem2(map) = solve(map, 20)

map = read_map(ARGS[1])
println(problem1(map))
println(problem2(map))