
function read_lock(lines)
    lock = Vector{Int}()
    for c in 1:length(lines[1])
        v = 0
        for r in 1:length(lines)
            if lines[r][c] == '#'
                v += 1 << (r - 1)
            end
        end
        push!(lock, v)
    end
    return lock
end

function read_locks(file_name)
    lines = readlines(file_name)
    return [read_lock(lines[i:i+6]) for i in 1:8:length(lines)]
end

function problem1(locks)
    result = 0
    for i in eachindex(locks)
        if locks[i][1] & 1 == 0
            continue
        end
        for j in eachindex(locks)
            if locks[j][1] & 1 != 0
                continue
            end
            if all(<=(127), locks[i] .+ locks[j])
                result += 1
            end
        end
    end
    return result
end

locks = read_locks(ARGS[1])
println(problem1(locks))