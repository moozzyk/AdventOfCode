read_map(file_name) =  collect.(readlines(file_name))

function find_antennas(map)
    antennas = Dict{Char, Vector{Tuple{Int, Int}}}()
    for r in eachindex(map), c in eachindex(map[r])
        point = map[r][c]
        if point != '.'
            positions = get!(antennas, point, Vector{Tuple{Int, Int}}())
            push!(positions, (r, c))
        end
    end
    return antennas
end

function find_antenna_anti_nodes(locations, max_pos)
    anti_nodes = Set{Tuple{Int, Int}}()
    for i in eachindex(locations), j in [1:i-1; i+1:length(locations)]
        l1, l2 = locations[i], locations[j]
        d = l1 .- l2
        r, c = l1 .+ d
        if (r in 1:max_pos && c in 1:max_pos)
            push!(anti_nodes, (r, c))
        end
    end
    return anti_nodes
end

function find_harmonic_anti_nodes(locations, max_pos)
    anti_nodes = Set{Tuple{Int, Int}}(locations)
    for i in eachindex(locations), j in [1:i-1; i+1:length(locations)]
        l1, l2 = locations[i], locations[j]
        d = l1 .- l2
        r, c = l1 .+ d
        while (r in 1:max_pos && c in 1:max_pos)
            push!(anti_nodes, (r, c))
            r += d[1]
            c += d[2]
        end
    end
    return anti_nodes
end

function find_all_anti_nodes(map, search_fn)
    antennas = find_antennas(map)
    return reduce(union, search_fn.(values(antennas), length(map)))
end

function find_anti_nodes(map, search_fn)
    return find_all_anti_nodes(map, search_fn) |> length
end

problem1(map) = find_anti_nodes(map, find_antenna_anti_nodes)
problem2(map) = find_anti_nodes(map, find_harmonic_anti_nodes)

map = read_map(ARGS[1])
println(problem1(map))
println(problem2(map))