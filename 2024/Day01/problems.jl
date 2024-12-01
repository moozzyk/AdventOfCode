# using Pkg
# Pkg.add("StatsBase")
using StatsBase

function read_file(filename)
    lines = readlines(filename)
    pairs = [Tuple(parse.(Int, split(line))) for line in lines]
    return pairs
end

function problem1(input)
    left = sort(map(t -> t[1], input))
    right = sort(map(t -> t[2], input))
    distances = [abs(y - x) for (x, y) in zip(left, right)]
    return distances |> sum
end

function problem1pipe(input)
    left = sort(map(t -> t[1], input))
    right = sort(map(t -> t[2], input))
    return (zip(left, right)
        |> z -> map(t -> abs(t[1] - t[2]), z)
        |> sum)
end

function problem2(input)
    left = map(t -> t[1], input)
    right_map = countmap(map(t -> t[2], input))
    similarity_scores = [k * get(right_map, k, 0) for (k) in left]
    return sum(similarity_scores)
end

function problem2pipe(input)
    left = map(t -> t[1], input)
    right_map = countmap(map(t -> t[2], input))
    return map(k -> k * get(right_map, k, 0), left) |> sum
end

input = read_file(ARGS[1])
println(problem1(input))
println(problem1pipe(input))
println(problem2(input))
println(problem2pipe(input))