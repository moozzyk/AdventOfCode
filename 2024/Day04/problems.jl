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

function problem1(word_matrix)
    result = 0
    dirs = Iterators.product([-1, 0, 1], [-1, 0, 1])
    for row in eachindex(word_matrix)
        for col in eachindex(word_matrix[row])
            result += [is_match(row, col, dir, "XMAS", word_matrix) for dir in dirs] |> sum
        end
    end
    return result
end

word_matrix = readlines(ARGS[1])
println(problem1(word_matrix))
