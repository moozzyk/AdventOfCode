
function explode_disk_map(disk_map)
    exploded_map = Vector{Int}()
    for i in eachindex(disk_map)
        item = isodd(i) ? div(i, 2) : -1
        append!(exploded_map, fill(item, disk_map[i]))
    end
    return exploded_map
end

function compute_checksum(disk_map)
    return map(((idx, item), ) -> (item == -1 ? 0 : item) * (idx - 1), enumerate(disk_map)) |> sum
end

function defragment(disk_map, defrag_fn)
    return explode_disk_map(disk_map) |> defrag_fn |> compute_checksum
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

function compute_empty_block_size(disk_map, i)
    block_size = 0
    while i <= length(disk_map) && disk_map[i] == -1
        block_size += 1
        i += 1
    end
    return block_size
end

function compute_file_size_backwards(disk_map, i)
    file_size = 0
    file_id = disk_map[i]
    while i > 0 && disk_map[i] == file_id
        file_size += 1
        i -= 1
    end
    return file_size
end

function find_empty_block(disk_map, block_size)
    for i in eachindex(disk_map)
        empty_block_size = compute_empty_block_size(disk_map, i)
        if (empty_block_size >= block_size)
            return i
        end
    end
    return typemax(Int)
end

function move_files(disk_map)
    right = length(disk_map)
    while right > 0
        file_size = compute_file_size_backwards(disk_map, right)
        empty_block_index = find_empty_block(disk_map, file_size)
        if empty_block_index < right
            for i in 0:file_size - 1
                disk_map[empty_block_index + i] = disk_map[right - i]
                disk_map[right - i] = -1
            end
        end
        right -= file_size
        while right > 0 && disk_map[right] == -1
            right -= 1
        end
    end
    return disk_map
end

problem1(disk_map) = defragment(disk_map, move_blocks)
problem2(disk_map) = defragment(disk_map, move_files)

disk_map = parse.(Int64, collect(readlines(ARGS[1])[1]))
println(problem1(disk_map))
println(problem2(disk_map))