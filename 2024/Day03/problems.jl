function read_file(filename)
    return readlines(filename)
end

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
    return vcat(process_line.(program)...) |> sum
end

program = read_file(ARGS[1])
println(problem1(program))
