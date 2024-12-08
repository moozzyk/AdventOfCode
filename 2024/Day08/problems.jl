read_map(file_name) =  collect.(readlines(file_name))

function find_antennas(map)
    antennas = Dict{Char, Vector{Tuple{Int, Int}}}()
    for r in eachindex(map)
        for c in eachindex(map[r])
            point = map[r][c]
            if point != '.'
                positions = get!(antennas, point, Vector{Tuple{Int, Int}}())
                push!(positions, (r, c))
            end
        end
    end
    return antennas
end

function find_antenna_anti_nodes(locations)
    anti_nodes = Set{Tuple{Int, Int}}()
    for i in eachindex(locations)
        for j in eachindex(locations)
            if i != j
                l1 = locations[i]
                l2 = locations[j]
                d = l1 .- l2
                push!(anti_nodes, l1 .+ d)
            end
        end
    end
    return anti_nodes
end

function find_all_anti_nodes(antennas)
    return reduce(union, find_antenna_anti_nodes.(values(antennas)))
end

function problem1(map)
    anti_nodes = find_antennas(map) |> find_all_anti_nodes
    filtered_anti_nodes = Set((r, c) for (r, c) in anti_nodes if r in 1:length(map) && c in 1:length(map[r]))
    return length(filtered_anti_nodes)
end

map = read_map(ARGS[1])
println(problem1(map))