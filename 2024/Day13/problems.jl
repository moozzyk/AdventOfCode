
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
    row1 = machine.buttonA[2] .* (machine.buttonA[1], machine.buttonB[1], machine.prize_position[1])
    row2 = machine.buttonA[1] .* (machine.buttonA[2], machine.buttonB[2], machine.prize_position[2])
    _, b, p = row1 .- row2

    if b != 0 && p % b == 0
        buttonB_presses = div(p, b)
        if (machine.prize_position[1] - machine.buttonB[1] * buttonB_presses) % (machine.buttonA[1]) == 0
            buttonA_presses = div(machine.prize_position[1] - machine.buttonB[1] * buttonB_presses, machine.buttonA[1])
            return (buttonA_presses, buttonB_presses)
        end
    end
    return(0, 0)
end

score((a, b)) = a * 3 + b

function problem1(machines)
    return sum(score.(filter(<=((100, 100)), play.(machines))))
end

mod_machine(machine) = Machine(machine.buttonA, machine.buttonB, machine.prize_position .+ 10000000000000)

function problem2(machines)
    return sum(score.(play.(mod_machine.(machines))))
end

machines = create_machines(ARGS[1])
println(problem1(machines))
println(problem2(machines))