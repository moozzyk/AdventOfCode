const UP = (-1, 0)
const DOWN = (1, 0)
const LEFT = (0, -1)
const RIGHT = (0, 1)

const DIRECTIONS = [UP, DOWN, LEFT, RIGHT]

function read_map(file_name)
    rows = [collect(line) for line in readlines(file_name)]
    return permutedims(hcat(rows...))
end

function safe_get(array, index, default)
    return checkbounds(Bool, array, index...) ? array[index...] : default
end

multiply_tuple(t) = reduce(*, t)