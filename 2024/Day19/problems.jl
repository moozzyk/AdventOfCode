function read_towels(file_name)
    lines = readlines(file_name)
    patterns = split(lines[1], ", ")
    return (lines[3:end], sort(patterns, by=length))
end

function is_design_possible(design, patterns, memo)
    if design == ""
        return true
    end

    if haskey(memo, design)
        return memo[design]
    end

    for pattern in patterns
        if startswith(design, pattern)
            if is_design_possible(design[length(pattern) + 1:end], patterns, memo)
                memo[design] = true
                return true
            end
        end
    end
    memo[design] = false
    return false
end

function problem1(designs, patterns)
    return map(design -> is_design_possible(design, patterns, Dict{String, Bool}()), designs) |> sum
end

(designs, patterns) = read_towels(ARGS[1])
println(problem1(designs, patterns))
