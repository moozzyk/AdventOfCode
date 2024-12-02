function read_file(filename)
    return readlines(filename) |> lines -> [parse.(Int, split(line)) for line in lines]
end

level_difference(levels) = [(levels[i] - levels[i + 1]) for i in 1:length(levels)-1]

is_same_sign(levels) = all(x -> x > 0, levels) || all(x -> x < 0, levels)

is_within_safe_range(levels) = all(x -> 0 < abs(x) < 4, levels)

is_safe(levels) = levels |> level_difference |> d -> (is_same_sign(d) && is_within_safe_range(d))

is_almost_safe(levels) = any(i -> is_safe(levels[1:end .!= i, :]), eachindex(levels))

problem1(reports) = is_safe.(reports) |> sum

problem2(reports) = is_almost_safe.(reports) |> sum

reports = read_file(ARGS[1])
println(problem1(reports))
println(problem2(reports))
