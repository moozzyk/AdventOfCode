include("../utils.jl")

function get_perimeter_segments(pos, map)
    (d -> safe_get(map, pos .+ d, '*') != map[pos...] ? 1 : 0).(DIRECTIONS) |> sum
end

function get_sides(pos, map)
    row, col = [pos...]
    sides = 0
    if row == 1 || map[row - 1, col] != map[row, col]
        if col == 1 || map[row, col - 1] != map[row, col]
            sides += 1
        end
    end
    if row == size(map, 1) || map[row + 1, col] != map[row, col]
        if col == 1 || map[row, col - 1] != map[row, col]
            sides += 1
        end
    end
    if col == 1 || map[row, col - 1] != map[row, col]
        if row == 1 || map[row - 1, col] != map[row, col]
            sides += 1
        end
    end
    if col == size(map, 2) || map[row, col + 1] != map[row, col]
        if row == 1 || map[row - 1, col] != map[row, col]
            sides += 1
        end
    end

    if row > 1 && col > 1 &&
        map[row, col - 1] == map[row - 1, col - 1] == map[row, col] &&
        map[row - 1, col] != map[row, col]
        sides += 1
    end

    if col > 1 && row < size(map, 1) &&
        map[row, col - 1] == map[row + 1, col - 1] == map[row, col] &&
        map[row + 1, col] != map[row, col]
        sides += 1
    end

    if row > 1 && col > 1 &&
        map[row - 1, col] == map[row - 1, col - 1] == map[row, col] &&
        map[row, col - 1] != map[row, col]
        sides += 1
    end

    if row > 1 && col < size(map, 2) &&
        map[row - 1, col] == map[row - 1, col + 1] == map[row, col] &&
        map[row, col + 1] != map[row, col]
        sides += 1
    end
    return sides
end

function visit(pos, plot_type, map, visited, fn)
    if pos in visited || safe_get(map, pos, '*') != plot_type
        return (0, 0)
    end
    push!(visited, pos)
    return reduce(.+, (d -> visit(pos .+ d, plot_type, map, visited, fn)).(DIRECTIONS)) .+ (fn(pos, map), 1)
end

function solve(map, fn)
    visited = Set{Tuple{Int, Int}}()
    return sum(
        multiply_tuple.(
            (pos -> visit(pos, map[pos...], map, visited, fn)).(Tuple.(CartesianIndices(map)))))
end

problem1(map) = solve(map, get_perimeter_segments)
problem2(map) = solve(map, get_sides)

map = read_map(ARGS[1])
println(problem1(map))
println(problem2(map))