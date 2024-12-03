function extract_muls(line)
    return [ match.match for match in eachmatch(r"mul\(\d+,\d+\)", line)]
end

function compute(mul)
    return parse.(Int64, [match.match for match in eachmatch(r"\d+", mul)]) |> prod
end

function process_line(line)
    return compute.(extract_muls(line))
end

function problem1(program)
    return process_line(program) |> sum
end

function problem2(program)
    return problem1(replace.(program, r"don't\(\).*?(do\(\)|$)" => ""))
end

program = join(readlines(ARGS[1]), "")
println(problem1(program))
println(problem2(program))