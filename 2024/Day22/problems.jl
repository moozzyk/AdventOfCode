function get_secret_numbers(file_name)
    return parse.(Int, readlines(file_name))
end

mix(secret_number, number) = xor(secret_number, number)
prune(number) = number % 16777216

function get_next_secret_number(secret_number)
    n = prune(mix(secret_number, secret_number << 6))
    n = prune(mix(n, n >> 5))
    n = prune(mix(n, n << 11))
    return n
end

function compute_secret_sequence(secret_number)
    result = Vector{Int}()
    push!(result, secret_number)
    for n in 1:2000
        push!(result, get_next_secret_number(result[end]))
    end
    return result
end

problem1(secret_numbers) = last.(compute_secret_sequence.(secret_numbers)) |> sum

function get_buyer_prices(secret_number)
    secret_numbers = compute_secret_sequence(secret_number)
    prices = map(x -> x % 10, secret_numbers)
    price_changes = Vector{Int}()
    for i in 2:length(prices)
        push!(price_changes, prices[i] - prices[i - 1])
    end

    result = Dict{Tuple{Int, Int, Int, Int}, Int}()
    for i in 4:length(prices) - 1
        seqeunce = tuple(price_changes[i - 3:i]...)
        if !haskey(result, seqeunce)
            result[seqeunce] = prices[i + 1]
        end
    end
    return result
end

function problem2(secret_numbers)
    result = Dict{Tuple{Int, Int, Int, Int}, Int}()
    for secret_number in secret_numbers
        buyer_prices = get_buyer_prices(secret_number)
        for (seq, price) in buyer_prices
            if haskey(result, seq)
                result[seq] += price
            else
                result[seq] = price
            end
        end
    end
    return values(result) |> maximum
end

secret_numbers = get_secret_numbers(ARGS[1])
println(problem1(secret_numbers))
println(problem2(secret_numbers))