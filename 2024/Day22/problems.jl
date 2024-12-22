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

function get_nth_secret_number(secret_number, n)
    while n > 0
        secret_number = get_next_secret_number(secret_number)
        n -= 1
    end
    return secret_number
end

function problem1(secret_numbers)
    return get_nth_secret_number.(secret_numbers, 2000) |> sum
end

secret_numbers = get_secret_numbers(ARGS[1])
println(problem1(secret_numbers))