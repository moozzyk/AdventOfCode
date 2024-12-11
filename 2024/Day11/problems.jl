
function get_num_stones(stone, num_blinks, cache)
    if num_blinks == 0 
        return 1
    end
    
    key = (stone, num_blinks)
    if !haskey(cache, key)
        if stone == "0"
            cache[key] = get_num_stones("1", num_blinks - 1, cache)
        elseif iseven(length(stone))
            stone1 = stone[1:div(length(stone), 2)]
            stone2 = stone[div(length(stone), 2) + 1:end]
            cache[key] = 
                get_num_stones(stone1, num_blinks - 1, cache) + 
                get_num_stones(string(parse(Int, stone2)), num_blinks - 1, cache)
        else 
            cache[key] = get_num_stones(string(parse(Int, stone) * 2024), num_blinks - 1, cache)
        end
    end
    return cache[key] 
end

function solve(stones, num_blinks) 
    cache = Dict{Tuple{String, Int}, Int}()
    return map(stone -> get_num_stones(stone, num_blinks, cache), stones) |> sum
end 

problem1(stones) = solve(stones, 25)
problem2(stones) = solve(stones, 75)

stones = split(readlines(ARGS[1])[1], " ")
println(problem1(stones))
println(problem2(stones))