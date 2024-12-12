include("../utils.jl")

function get_perimeter_segments(pos, map) 
    (d -> safe_get(map, pos .+ d, '*') != map[pos...] ? 1 : 0).(DIRECTIONS) |> sum
end

function visit(pos, plot_type, map, visited)
    if pos in visited || safe_get(map, pos, '*') != plot_type
        return (0, 0)
    end
    push!(visited, pos)
    return reduce(.+, (d -> visit(pos .+ d, plot_type, map, visited)).(DIRECTIONS)) .+ 
        (get_perimeter_segments(pos, map), 1)
end

function problem1(map)
    return sum(
        multiply_tuple.(
            (pos -> visit(pos, map[pos...], map, Set{Tuple{Int, Int}}())).(Tuple.(CartesianIndices(map)))))
end

map = read_map(ARGS[1])
println(problem1(map))