mutable struct CPU
    A::Int
    B::Int
    C::Int
end

function initialize(file_name)
    get_register_value(line) = parse(Int, split(line, ": ")[2])
    lines = readlines(file_name)
    cpu = CPU(get_register_value.(lines[1:3])...)
    program = parse.(Int, split(split(lines[5], ": ")[2], ','))
    return (cpu, program)
end

function execute(cpu, program)
    function get_combo_operand(operand)
        if operand < 4
            return operand
        elseif operand == 4
            return cpu.A
        elseif operand == 5
            return cpu.B
        elseif operand == 6
            return cpu.C
        end
    end

    output = Vector{Int}()
    IP = 1
    while true
        if IP > length(program)
            return output
        end
        opcode = program[IP]
        operand = program[IP + 1]
        if opcode == 0 #div
            cpu.A = div(cpu.A, (1 << get_combo_operand(operand)))
        elseif opcode == 1 # bxl
            cpu.B = xor(cpu.B, operand)
        elseif opcode == 2 # bst
            cpu.B = get_combo_operand(operand) % 8
        elseif opcode == 3 # jnz
            if cpu.A != 0
                IP = operand + 1
                continue
            end
        elseif opcode == 4 # bxc
            cpu.B = xor(cpu.B, cpu.C)
        elseif opcode == 5 # out
            push!(output, get_combo_operand(operand) % 8)
        elseif opcode == 6 # bdv
            cpu.B = div(cpu.A, (1 << get_combo_operand(operand)))
        elseif opcode == 7 # cdv
            cpu.C = div(cpu.A, (1 << get_combo_operand(operand)))
        end
        IP += 2
    end
end

function problem1(cpu, program)
    return join(execute(cpu, program), ",")
end

cpu, program = initialize(ARGS[1])
println(problem1(cpu, program))
