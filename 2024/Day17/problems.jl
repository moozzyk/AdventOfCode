function initialize(file_name)
    get_register_value(line) = parse(Int, split(line, ": ")[2])
    lines = readlines(file_name)
    regA = get_register_value.(lines[1:3])[1]
    program = parse.(Int, split(split(lines[5], ": ")[2], ','))
    return (regA, program)
end

function execute(regA, program)
    function get_combo_operand(operand)
        if operand < 4
            return operand
        elseif operand == 4
            return regA
        elseif operand == 5
            return regB
        elseif operand == 6
            return regC
        end
    end

    output = Vector{Int}()
    IP = 1
    regB = 0
    regC = 0
    while true
        if IP > length(program)
            return output
        end
        opcode = program[IP]
        operand = program[IP + 1]
        if opcode == 0 #div
            regA = div(regA, (1 << get_combo_operand(operand)))
        elseif opcode == 1 # bxl
            regB = xor(regB, operand)
        elseif opcode == 2 # bst
            regB = get_combo_operand(operand) % 8
        elseif opcode == 3 # jnz
            if regA != 0
                IP = operand + 1
                continue
            end
        elseif opcode == 4 # bxc
            regB = xor(regB, regC)
        elseif opcode == 5 # out
            push!(output, get_combo_operand(operand) % 8)
        elseif opcode == 6 # bdv
            regAB = div(regA, (1 << get_combo_operand(operand)))
        elseif opcode == 7 # cdv
            regC = div(regA, (1 << get_combo_operand(operand)))
        end
        IP += 2
    end
end

function problem1(regA, program)
    return join(execute(regA, program), ",")
end

function get_cycle(program)
    [ execute(regA, program)[1] for regA in 0:1023]
end

function find_seed(sequence, seed, cycle)
    if length(sequence) == 0
        return seed
    end
    value = pop!(sequence)
    result = -1
    for i in 1:8
        new_seed_candidate = (seed * 8) + i
        if cycle[new_seed_candidate % length(cycle)] == value
            result = find_seed(sequence, new_seed_candidate - 1, cycle)
            if result != -1
                break
            end
        end
    end
    push!(sequence, value)
    return result
end

function problem2(program)
    cycle = get_cycle(program)
    value = pop!(program)
    for idx in findall(==(value), cycle)
        result = find_seed(program, idx - 1, cycle)
        if result != -1
            return result
        end
    end
    throw("No solution found")
end

regA, program = initialize(ARGS[1])
println(problem1(regA, program))
println(problem2(program))