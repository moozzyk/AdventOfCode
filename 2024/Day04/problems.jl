function is_match(row, col, (dr, dc), str, word_matrix)
    for letter in str
        if row < 1 || col < 1 || row > length(word_matrix) || col > length(word_matrix[row])
            return false
        end
        if word_matrix[row][col] != letter
            return false
        end
        row += dr
        col += dc
    end
    return true
end

function is_XMAS(row, col, word_matrix)
    dirs = Iterators.product([-1, 0, 1], [-1, 0, 1])
    return [is_match(row, col, dir, "XMAS", word_matrix) for dir in dirs] |> sum
end

function is_X_MAS(row, col, word_matrix)
    return (
        (
            is_match(row, col, (1, 1), "MAS", word_matrix) ||
            is_match(row, col, (1, 1), "SAM", word_matrix)
        )
        &&
        (
            is_match(row, col + 2, (1, -1), "MAS", word_matrix) ||
            is_match(row, col + 2, (1, -1), "SAM", word_matrix)
        )
    )
end

function visit(m, fn)
    return [fn(r, c, m) for r in eachindex(m), c in eachindex(m[1])] |> sum
end

problem1(word_matrix) = visit(word_matrix, is_XMAS)
problem2(word_matrix) = visit(word_matrix, is_X_MAS)

word_matrix = readlines(ARGS[1])
println(problem1(word_matrix))
println(problem2(word_matrix))
