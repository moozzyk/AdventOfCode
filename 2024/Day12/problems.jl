function read_map(file_name)
    rows = [collect(line) for line in readlines(file_name)]
    return permutedims(hcat(rows...))
end

function get_perimeter_segments(pos, map) 
    row, col = [pos...]
    perimeter = 0 
    if row == 1 || map[row - 1, col] != map[row, col]
        perimeter += 1
    end 
    if row == size(map, 1) || map[row + 1, col] != map[row, col]
        perimeter +=1
    end
    if col == 1 || map[row, col - 1] != map[row, col]
        perimeter += 1
    end 
    if col == size(map, 2) || map[row, col + 1] != map[row, col]
        perimeter += 1
    end 
    return perimeter
end

function visit(pos, plot_type, map, visited)
    if pos in visited || !checkbounds(Bool, map, pos...) || map[pos...] != plot_type
        return (0, 0)
    end

    result = [(get_perimeter_segments(pos, map), 1)]
    push!(visited, pos)
    for delta in [(-1, 0), (1, 0), (0, -1), (0, 1)] 
        push!(result, visit(pos .+ delta, plot_type, map, visited))
    end
    return reduce(.+, result)
end

function problem1(map)
    visited = Set{Tuple{Int, Int}}()
    println(map)
    result = Vector{Int}() # Vector{Tuple{Int, Int}}()

    for plot_pos in CartesianIndices(map)
        pos = Tuple(plot_pos)
        if !(pos in visited)
            println(pos, " ", map[pos...])
            res = visit(pos, map[pos...], map, visited)
            push!(result, res[1] * res[2])
        end
    end
    println(result)
    return sum(result)
end


map = read_map(ARGS[1])
println(problem1(map))