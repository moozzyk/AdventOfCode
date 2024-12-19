function read_towels(file_name)
    lines = readlines(file_name)
    patterns = split(lines[1], ", ")
    return (lines[3:end], sort(patterns, by=length))
end

function num_possible_designs(design, patterns, memo)
    if design == ""
        return 1
    end

    if !haskey(memo, design)
        num_designs = 0
        for pattern in patterns
            if startswith(design, pattern)
                num_designs += num_possible_designs(design[length(pattern) + 1:end], patterns, memo)
            end
        end
        memo[design] = num_designs
    end
    return memo[design]
end

function problem1(designs, patterns)
    return map(design -> num_possible_designs(design, patterns, Dict{String, Integer}()) != 0, designs) |> sum
end

function problem2(designs, patterns)
    return map(design -> num_possible_designs(design, patterns, Dict{String, Integer}()), designs) |> sum
end

(designs, patterns) = read_towels(ARGS[1])
println(problem1(designs, patterns))
println(problem2(designs, patterns))
