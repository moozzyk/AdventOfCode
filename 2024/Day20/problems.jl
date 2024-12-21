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

function problem1(start_pos, end_pos, map)
    path = Set{Tuple{Int, Int}}()
    distances = find_path(end_pos, start_pos, map, path)
    normal_path_length = distances[start_pos]
    result = Vector{Tuple{Int, Tuple{Int, Int}, Tuple{Int, Int}}}()

    for (pos, distance) in distances
        for dir in DIRECTIONS
            (r, c) = pos .+ (2 .* dir)
            if safe_get(map, (r, c), '#') == '#'
                continue
            end
            steps_to_end = get(distances, (r, c), Inf)
            if steps_to_end != Inf
                steps = normal_path_length - distances[pos] + steps_to_end + 2
                if steps < normal_path_length
                    push!(result, (normal_path_length - steps, (pos .+ dir), (r, c)))
                end
            end

        end
    end
    return length(filter((l) -> l[1] >= 100, result))
    # return sort(result)

    # result = Vector{Tuple{Int, Tuple{Int, Int}, Tuple{Int, Int}}}()
    # checked = Set{Tuple{Int, Int}}()

    # for pos in path
    #     for dir in DIRECTIONS
    #         (r, c) = pos .+ dir
    #         if (r, c) in checked
    #             continue
    #         end
    #         push!(checked, (r, c))
    #         if r > 1 && c > 1 && r < size(map, 1) && c < size(map, 2) && map[r, c] == '#'
    #             t = map[r, c]
    #             map[r, c] = '.'
    #             path_length = find_path(start_pos, end_pos, map)
    #             if (path_length < initial_path_length)
    #                 push!(result, (initial_path_length - path_length, (r, c), (r, c + 1)))
    #             end
    #             map[r, c] = t
    #         end
    #     end
    # end
    # return length(filter((l) -> l[1] >= 100, result))
end

map = read_map(ARGS[1])
start_pos = find_in_map('S', map)
end_pos = find_in_map('E', map)
println.(problem1(start_pos, end_pos, map))