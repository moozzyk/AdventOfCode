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

function problem1(nodes)
    result = Set{Tuple{String, String, String}}()
    foreach(n -> find_group(nodes, n, result), keys(nodes))
    return any.(n -> n[1] == 't', result) |> sum
end

function find_maximal_clique(vertices, edges, start_vertex)
    clique = Set{String}([start_vertex])
    for candidate in vertices
        belongs = true
        for v in clique
            if !((candidate, v) in edges)
                belongs = false
                break
            end
        end
        if belongs
            push!(clique, candidate)
            delete!(vertices, candidate)
        end
    end
    return join(sort(collect(clique)), ",")
end

function problem2(nodes)
    edges = Set{Tuple{String, String}}()
    for (from, targets) in nodes
        union!(edges, Set(map(to -> (from, to), targets)))
    end
    passwords = [find_maximal_clique(Set([keys(nodes)...]), edges, v) for v in keys(nodes)]
    return sort(passwords, by=length) |> last
end

nodes = read_lan(ARGS[1])
println(problem1(nodes))
println(problem2(nodes))