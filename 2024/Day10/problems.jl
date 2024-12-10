function read_map(file_name)
    rows = [parse.(Int, collect(line)) for line in readlines(file_name)]
    return hcat(rows...)'
end

function compute_trailhead_score(pos, map, visited_peaks)
    if (map[pos...] == 9)
        if visited_peaks != nothing
            if pos in visited_peaks 
                return 0
            end
            push!(visited_peaks, pos)
        end
        return 1
    end
    
    score = 0
    for delta in [(-1, 0), (1, 0), (0, -1), (0, 1)] 
        new_pos = pos .+ delta
        if checkbounds(Bool, map, new_pos...) && map[new_pos...] == map[pos...] + 1 
            score += compute_trailhead_score(new_pos, map, visited_peaks)
        end
    end
    return score
end 

function solve(map, track_peaks_fn)
    return [compute_trailhead_score(Tuple(pos), map, track_peaks_fn()) for pos in findall(==(0), map)] |> sum
end

problem1(map) = solve(map, () -> Set{Tuple{Int, Int}}())
problem2(map) = solve(map, () -> nothing)

map = read_map(ARGS[1])
println(problem1(map))
println(problem2(map))