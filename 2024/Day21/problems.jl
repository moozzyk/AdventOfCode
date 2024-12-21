
include("../utils.jl")

MOVES = Dict(UP => '^', DOWN => 'v', LEFT => '<', RIGHT => '>')

NUM_KEYPAD = Dict(
    '7' => (1, 1), '8' => (1, 2), '9' => (1, 3),
    '4' => (2, 1), '5' => (2, 2), '6' => (2, 3),
    '1' => (3, 1), '2' => (3, 2), '3' => (3, 3),
                   '0' => (4, 2), 'A' => (4, 3))

DIR_KEYPAD = Dict(
                   '^' => (1, 2), 'A' => (1, 3),
    '<' => (2, 1), 'v' => (2, 2), '>' => (2, 3))

distance(p1, p2) = sum(abs.(p2 .- p1))
compute_complexity(code, seq) = parse(Int, replace(code, "A" => "" )) * length(seq)

function get_dir_keypad_path(start_key, end_key)
    valid_positions = values(DIR_KEYPAD)

    path = ""
    pos = DIR_KEYPAD[start_key]
    end_pos = DIR_KEYPAD[end_key]
    while pos != end_pos
        for dir in [DOWN, LEFT, RIGHT, UP]
            new_pos = pos .+ dir
            if new_pos in valid_positions && distance(new_pos, end_pos) < distance(pos, end_pos)
                path *= MOVES[dir]
                pos = new_pos
                break
            end
        end
    end
    return path
end

function get_dir_keypad_seq(code)
    sequences = ""
    key = 'A'
    for next_key in code
        sequences *= get_dir_keypad_path(key, next_key) * 'A'
        key = next_key
    end
    return sequences
end

function find_dir_num_pad_seq(pos, path, idx, seq, results)
    if (idx > length(seq))
        push!(results, path)
        return
    end
    target = NUM_KEYPAD[seq[idx]]
    if pos == target
        find_dir_num_pad_seq(pos, path * 'A', idx + 1, seq, results)
        return
    end

    valid_positions = values(NUM_KEYPAD)
    for dir in [DOWN, LEFT, RIGHT, UP]
        next_pos = pos .+ dir
        if next_pos in valid_positions && distance(next_pos, target) < distance(pos, target)
            find_dir_num_pad_seq(next_pos, path * MOVES[dir], idx, seq, results)
        end
    end
end

function solve(code)
    num_key_pad_res = Vector{}()
    find_dir_num_pad_seq((4, 3), "", 1, code, num_key_pad_res)
    res = [s |> get_dir_keypad_seq |> get_dir_keypad_seq for s in num_key_pad_res]
    return compute_complexity.(code, res) |> minimum
end

problem1(codes) = solve.(codes) |> sum
codes = readlines(ARGS[1])
println(problem1(codes))