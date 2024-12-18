# import Pkg; Pkg.add("DataStructures")
using DataStructures

include("../utils.jl")

function read_coordinates(file_name)
    return map(l -> parse.(Int, (l[1], l[2])), parse_line.(readlines(file_name), r"(\d+)"))
end

function find_path(coordinates)
    max_coord = 70
    coords = Set(coordinates)
    visited = Set{Tuple{Int, Int}}()
    q = Queue{Tuple{Tuple{Int, Int}, Int}}()
    enqueue!(q, ((0, 0), 0))
    while length(q) > 0
        (c, steps) = dequeue!(q)
        (x, y) = c
        if  (x, y) in visited || x < 0 || y < 0 || x > max_coord || y > max_coord
            continue
        end
        if c == (max_coord, max_coord)
            return steps
        end
        push!(visited, c)
        for dir in DIRECTIONS
            new_coord = c .+ dir
            if !(new_coord in coords || new_coord in visited)
                enqueue!(q, ((new_coord), steps + 1))
            end
        end
    end
    return nothing
end

problem1(coordinates) = find_path(coordinates[1:1024])

coordinates = read_coordinates(ARGS[1])
println(problem1(coordinates))