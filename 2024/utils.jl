const UP = (-1, 0)
const DOWN = (1, 0)
const LEFT = (0, -1)
const RIGHT = (0, 1)

const DIRECTIONS = [UP, DOWN, LEFT, RIGHT]

function lines_to_char_map(lines)
    return permutedims(hcat(collect.(lines)...))
end

read_map(file_name::String) = readlines(file_name) |> read_map

function draw_map(map)
    println.(join.(eachrow(map)))
end

function safe_get(array, index, default)
    return checkbounds(Bool, array, index...) ? array[index...] : default
end

multiply_tuple(t) = reduce(*, t)

parse_line(line, regex) = [match.match for match in eachmatch(regex, line)]

parse_line_ints(line, regex) = parse.(Int, parse_line(line, regex))

find_in_map(c, map) = filter(p -> map[p...] == c, Tuple.(CartesianIndices(map)))[1]

