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
    result = 1
    for op1 in [<, >], op2 in [<, >]
        result *= sum(is_in_quadrant.(new_pos, op1, op2, width, height))
    end 
    return result
end

robots = create_robots(ARGS[1])
println(problem1(robots, 100, parse(Int, ARGS[2]), parse(Int, ARGS[3])))
