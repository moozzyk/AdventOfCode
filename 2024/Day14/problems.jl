include("../utils.jl")

struct Robot 
    pos::Tuple{Int, Int}
    vel::Tuple{Int, Int}
end

function create_robots(file_name)
    lines = readlines(file_name)
    map(n -> Robot((n[1], n[2]), (n[3], n[4])), parse_line_ints.(lines, r"(-?\d+)"))
end

function move_robot(robot, seconds, width, height) 
    new_x, new_y = robot.pos .+ seconds .* robot.vel
    return ((new_x + seconds * width) % width, (new_y + seconds * height) % height)
end

function is_in_quadrant((x, y), op1, op2, width, height)
    return op1(x, div(width, 2)) && op2(y, div(height, 2))
end 

function problem1(robots, seconds, width, height) 
    new_pos = move_robot.(robots, seconds, width, height)
    quadrant_results = vec([(is_in_quadrant.(new_pos, op1, op2, width, height)) for op1 in [<, >], op2 in [<, >]])
    return map(sum, quadrant_results) |> prod
end

function draw(robots, width, height) 
    set = Set(robots)
    for y in 0:height - 1
        for x in 0:width - 1
            if (x, y) in set 
                print("*")
            else 
                print(".")
            end
        end
        println()
    end
end

function is_tree(robots, width, height)
    set = Set(robots)
    streak = 0
    for x in 1:width - 1, y in 0:height - 1
        if (x, y) in set 
            streak += 1
        else
            streak = 0
        end
        if streak == 20
            return true
        end
    end
    return false 
end

function problem2(robots, width, height)
    second = 0
    while true
        new_pos = move_robot.(robots, second, width, height)
        if is_tree(new_pos, width, height)
            draw(new_pos, width, height)
            println(second)
            break
        end
        second += 1
    end
end

robots = create_robots(ARGS[1])
width = parse(Int, ARGS[2])
height = parse(Int, ARGS[3])
println(problem1(robots, 100, width, height))
problem2(robots, width, height)
