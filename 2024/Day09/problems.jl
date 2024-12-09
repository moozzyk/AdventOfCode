
function explode_disk_map(disk_map)
    exploded_map = Vector{Int}()
    for i in eachindex(disk_map)
        item = isodd(i) ? div(i, 2) : -1
        append!(exploded_map, fill(item, disk_map[i]))
    end
    return exploded_map
end

function move_blocks(disk_map)
    left = 1
    right = length(disk_map)

    while left < right
        if disk_map[left] == -1
            disk_map[left] = disk_map[right]
            disk_map[right] = -1
            right -= 1
            while disk_map[right] == -1
                right -= 1
            end
        end
        left += 1
    end
    return disk_map
end

function compute_checksum(disk_map)
    return map(((idx, item), ) -> item * (idx - 1), enumerate(filter(i -> i >= 0, disk_map))) |> sum
end

function problem1(disk_map)
    explode_disk_map(disk_map) |> move_blocks |> compute_checksum
end

disk_map = parse.(Int64, collect(readlines(ARGS[1])[1]))
println(problem1(disk_map))