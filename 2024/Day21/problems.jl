
include("../utils.jl")

MOVES = Dict(UP => '^', DOWN => 'v', LEFT => '<', RIGHT => '>')

NUM_KEYPAD = Dict(
    '7' => (1, 1), '8' => (1, 2), '9' => (1, 3),
    '4' => (2, 1), '5' => (2, 2), '6' => (2, 3),
    '1' => (3, 1), '2' => (3, 2), '3' => (3, 3),
                   '0' => (4, 2), 'A' => (4, 3))

DIR_KEYPAD = Dict(
                   '^' => (1, 2), 'A' => (1, 3),
    '<' => (2, 1), 'v' => (2, 2), '>' => (2, 3))

distance(p1, p2) = sum(abs.(p2 .- p1))

compute_complexity(code, seq_length) = parse(Int, replace(code, "A" => "" )) * seq_length

function generate_paths(pos, dst, path, keypad)
    if pos == dst
        return [path]
    end

    result = Vector{String}()
    for dir in [DOWN, LEFT, RIGHT, UP]
        next_pos = pos .+ dir
        if next_pos in values(keypad) && distance(next_pos, dst) < distance(pos, dst)
            append!(result, generate_paths(next_pos, dst, path * MOVES[dir], keypad))
        end
    end
    return result
end

function path_generator(keypad)
    return Dict(
        (src, dst) => generate_paths(keypad[src], keypad[dst], "", keypad)
        for src in keys(keypad), dst in keys(keypad))
end

function generate_all_paths()
    return merge(path_generator(NUM_KEYPAD), path_generator(DIR_KEYPAD))
end

function find_shortest_sequence(s, depth, memo)
    if depth == 0
        return length(s) + 1 # account for trailing 'A'
    end

    key = (s, depth)
    if !haskey(memo, key)
        s = 'A' * s * 'A'
        memo[key] = [
            minimum(find_shortest_sequence.(paths[s[i], s[i + 1]], depth - 1, Ref(memo)))
            for i in (1:length(s) - 1)
        ] |> sum
    end

    return memo[key]
end

function compute_length(code, depth, memo = Dict{Tuple{String, Int}, Int}())
    code = 'A' * code
    return [
        minimum(find_shortest_sequence.(paths[(code[i], code[i + 1])], depth, Ref(memo)))
        for i in (1:length(code) -1)
    ] |> sum
end

function solve(code, depth)
    return compute_complexity(code, compute_length(code, depth))
end

problem1(codes) = solve.(codes, 2) |> sum
problem2(codes) = solve.(codes, 25) |> sum

paths = generate_all_paths()
codes = readlines(ARGS[1])
println(problem1(codes))
println(problem2(codes))
