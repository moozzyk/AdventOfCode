
struct Machine
    buttonA::Tuple{Int, Int}
    buttonB::Tuple{Int, Int}
    prize_position::Tuple{Int, Int}
end

function create_machine(lines, idx)
    parse_line(line) = Tuple(parse.(Int, match.match for match in eachmatch(r"(\d+)", line)))
    return Machine(parse_line.(lines[idx:idx + 2])...)
end 

function create_machines(file_name) 
    machines = Vector{Machine}()
    lines = readlines(file_name)
    for idx in 1:4:length(lines)
        push!(machines, create_machine(lines, idx))
    end
    return machines
end

function play(machine) 
    result = Vector{Int}()
    for a in 1:100
        for b in 1:100
            if (a .* machine.buttonA) .+ (b .* machine.buttonB) == machine.prize_position
                push!(result, a * 3 + b)
            end
        end
    end
    return length(result) == 0 ? Inf : minimum(result)
end


problem1(machines) = filter(isfinite, play.(machines)) |> sum

machines = create_machines(ARGS[1])
println(problem1(machines))
