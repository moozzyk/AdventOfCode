function read_file(filename)
    lines = readlines(filename)
    reports = [parse.(Int, split(line)) for line in lines]
    return reports
end

level_difference(levels) = [(levels[i] - levels[i + 1]) for i in 1:length(levels)-1]

is_same_sign(levels) = all(x -> x > 0, levels) || all(x -> x < 0, levels)

is_within_safe_range(levels) = all(x -> 0 < abs(x) < 4, levels)

is_safe(levels) = is_same_sign(levels) && is_within_safe_range(levels)

function problem1(reports)
    return (map(level_difference, reports)
        |> differences -> map(is_safe, differences)
        |> sum
    )
end

reports = read_file(ARGS[1])
println(problem1(reports))

