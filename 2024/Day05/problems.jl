function create_page_order_map(lines)
    page_order_map = Dict{Int, Set{Int}}()
    for line in lines
        from, to = parse.(Int, split(line, "|"))
        pages = get!(page_order_map, from, Set{Int}())
        push!(pages, to)
    end
    return page_order_map
end

function readfile(name)
    parse_updates(updates) = parse.(Int, split(updates, ","))
    lines = readlines(name)
    page_order_map = create_page_order_map(filter(l -> occursin("|", l), lines))
    updates = parse_updates.(filter(l -> occursin(",", l), lines))
    return (page_order_map, updates)
end

function is_order_correct(update, page_order_map)
    for i in 1:length(update) - 1
        to_pages = get(page_order_map, update[i], Set{Int}())
        for j in i + 1:length(update)
            if !in(update[j], to_pages)
                return false
            end
        end
    end
    return true
end

function retrieve_correct_page(update, page_order_map)
    if is_order_correct(update, page_order_map)
        return update[div(length(update), 2) + 1]
    end
    return 0
end

function problem1(page_order_map, updates)
    [retrieve_correct_page(update, page_order_map) for update in updates] |> sum
end

function retrieve_correct_page_if_ordered(update, page_order_map)
    for i in eachindex(update)
        to_pages = get(page_order_map, update[i], Set{Int}())
        num_followers = 0
        for j in [1:i-1; i+1:length(update)]
            if update[j] ∈ to_pages
                num_followers += 1
            end
        end
        if num_followers == div(length(update), 2)
            return update[i]
        end
    end
end

function problem2(page_order_map, updates)
    result = 0
    for update in updates
        if !is_order_correct(update, page_order_map)
            result += retrieve_correct_page_if_ordered(update, page_order_map)
        end
    end
    return result
end

(page_order_map, updates) = readfile(ARGS[1])
println(problem1(page_order_map, updates))
println(problem2(page_order_map, updates))

