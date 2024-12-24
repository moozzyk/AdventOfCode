include("../utils.jl")

function parse_circuit_line(line)
    return tuple(match(r"(\S+) (AND|OR|XOR) (\S+) -> (\S+)", line).captures...)
end

function parse_input_line(line)
    captures = match(r"(\S+): (\d)", line).captures
    return (captures[1], parse(Int, captures[2]))
end

function read_input(file_name)
    lines = readlines(file_name)
    pivot = findfirst(==(""), lines)
    inputs = parse_input_line.(lines[1:pivot - 1])
    circuit = parse_circuit_line.(lines[(pivot + 1):end])
    return (inputs, circuit)
end

function create_state(inputs)
    state = Dict{String, Int}()
    for i in inputs
        state[i[1]] = i[2]
    end
    return state
end

function get_value(circuit, state, gate)
    if !haskey(state, gate)
        idx = findfirst(c -> last(c) == gate, circuit)
        evaluate(circuit, state, circuit[idx])
    end
    return state[gate]
end

function evaluate(circuit, state, connection)
    (g1, op, g2, tg) = connection
    v1 = get_value(circuit, state, g1)
    v2 = get_value(circuit, state, g2)
    if op == "AND"
        state[tg] = v1 & v2
    elseif op == "XOR"
        state[tg] = xor(v1, v2)
    elseif op == "OR"
        state[tg] = v1 | v2
    else
        throw("Unexpected operator")
    end
end

function problem1(inputs, circuit)
    state = create_state(inputs)
    for connection in circuit
        if last(connection)[1] == 'z'
            evaluate(circuit, state, connection)
        end
    end
    result = sort(collect(filter(((g, _),) -> g[1] == 'z', state)))
    return map(((index, (_, value)),) -> value << (index - 1),  enumerate(result)) |> sum
end

(inputs, circuit) = read_input(ARGS[1])
println(problem1(inputs, circuit))