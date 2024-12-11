
function blink(stones)
    result = Vector{String}()
    for s in stones
        if s == "0"
            push!(result, "1")
        elseif iseven(length(s))
            s1 = s[1:div(length(s), 2)]
            s2 = s[div(length(s), 2) + 1:end]
            push!(result, s1)
            push!(result, string(parse(Int, s2)))
        else
            push!(result, string(parse(Int, s) * 2024))
        end
    end
    return result
end

function problem1(stones)
    for i in 1:25
        stones = blink(stones)
    end
    return length(stones)
end 

stones = split(readlines(ARGS[1])[1], " ")
println(problem1(stones))