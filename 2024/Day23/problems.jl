function read_lan(file_name)
    lines = readlines(file_name)
    pairs = map(e -> tuple(e...), split.(lines, "-"))
    nodes = Dict{String, Vector{String}}()
    for (c1, c2) in pairs
        if !haskey(nodes, c1)
            nodes[c1] = Vector{String}()
        end
        push!(nodes[c1], c2)
        if !haskey(nodes, c2)
            nodes[c2] = Vector{String}()
        end
        push!(nodes[c2], c1)
    end
    return nodes
end

function find_group(nodes, n1, result)
    for i in 1:length(nodes[n1]) - 1
        n2 = nodes[n1][i]
        for j in (i + 1):length(nodes[n1])
            n3 = nodes[n1][j]
            if n2 in nodes[n3]
                push!(result, tuple(sort([n1, n2, n3])...))
            end
        end
    end
end

has_t((n1, n2, n3)) = n1[1] == 't' || n2[1] == 't' || n3[1] == 't'

function problem1(nodes)
    result = Set{Tuple{String, String, String}}()
    for n in keys(nodes)
        find_group(nodes, n, result)
    end

    return has_t.(result) |> sum
end

nodes = read_lan(ARGS[1])
println(problem1(nodes))