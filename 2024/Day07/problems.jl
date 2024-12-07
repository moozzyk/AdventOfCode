function parse_line(line)
    result, factors = split(line, ":")
    return (parse(Int, result), parse.(Int, split(factors, " ", keepempty=false)))
end

read_equations(file_name) = parse_line.(readlines(file_name))
tail(v) = length(v) == 1 ? [] : v[2:end]

function is_valid(accumulator, target, factors, operations)
    if accumulator > target
        return false
    end
    if length(factors) == 0
        return accumulator == target
    end
    for op in operations
        if is_valid(op(accumulator, factors[1]), target, tail(factors), operations)
            return true
        end
    end
    return false
end

function solve(equations, operations)
    result = 0
    for (target, factors) in equations
        if is_valid(factors[1], target, tail(factors), operations)
            result += target
        end
    end
    return result
end

function problem1(equations)
    solve(equations, [+, *])
end

function problem2(equations)
    concat(accumulator, factor) = accumulator * (10 ^ length(string(factor))) + factor
    solve(equations, [+, *, concat])
end

equations = read_equations(ARGS[1])
println(problem1(equations))
println(problem2(equations))