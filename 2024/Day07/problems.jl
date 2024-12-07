function parse_line(line)
    result, factors = split(line, ":")
    return (parse(Int, result), parse.(Int, split(factors, " ", keepempty=false)))
end

read_equations(file_name) = parse_line.(readlines(file_name))
tail(v) = length(v) == 1 ? [] : v[2:end]

function solve(accumulator, target, factors)
    if accumulator > target
        return false
    end
    if length(factors) == 0
        return accumulator == target
    end
    return (
        solve(accumulator + factors[1], target, tail(factors)) ||
        solve(accumulator * factors[1], target, tail(factors)))
end

function problem1(equations)
    result = 0
    for (target, factors) in equations
        if solve(factors[1], target, tail(factors))
            result += target
        end
    end
    return result
end

equations = read_equations(ARGS[1])
println(problem1(equations))