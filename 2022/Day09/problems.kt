import java.io.File

data class Point(val x: Int, val y: Int) {}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(problem1(lines))
    println(problem2(lines))
}

fun problem1(moves: List<String>): Int {
    return solve(Array(2) { Point(0, 0) }, moves)
}

fun problem2(moves: List<String>): Int {
    return solve(Array(10) { Point(0, 0) }, moves)
}

fun solve(rope: Array<Point>, moves: List<String>): Int {
    var regex = """(.) (\d+)""".toRegex()
    val visited = mutableListOf(Point(0, 0))
    for (m in moves) {
        val (direction, steps) = regex.matchEntire(m)!!.destructured
        for (s in 1..steps.toInt()) {
            rope[0] = moveHead(rope[0], direction.first())
            for (i in 1 until rope.size) {
                rope[i] = moveTail(rope[i - 1], rope[i])
            }
            visited.add(rope.last())
        }
    }
    println(visited.size)
    return visited.toSet().size
}

fun moveHead(headPos: Point, direction: Char): Point {
    when (direction) {
        'R' -> return Point(headPos.x + 1, headPos.y)
        'L' -> return Point(headPos.x - 1, headPos.y)
        'U' -> return Point(headPos.x, headPos.y - 1)
        'D' -> return Point(headPos.x, headPos.y + 1)
        else -> throw Exception("Unexpected direction: ${direction}")
    }
}

fun moveTail(headPos: Point, tailPos: Point): Point {
    if (tailPos.x == headPos.x - 2) {
        if (tailPos.y == headPos.y - 1 || tailPos.y == headPos.y - 2)
                return Point(tailPos.x + 1, tailPos.y + 1)
        if (tailPos.y == headPos.y + 1 || tailPos.y == headPos.y + 2)
                return Point(tailPos.x + 1, tailPos.y - 1)
        return Point(tailPos.x + 1, tailPos.y)
    } else if (tailPos.x == headPos.x + 2) {
        if (tailPos.y == headPos.y - 1 || tailPos.y == headPos.y - 2)
                return Point(tailPos.x - 1, tailPos.y + 1)
        if (tailPos.y == headPos.y + 1 || tailPos.y == headPos.y + 2)
                return Point(tailPos.x - 1, tailPos.y - 1)
        return Point(tailPos.x - 1, tailPos.y)
    } else if (tailPos.y == headPos.y - 2) {
        if (tailPos.x == headPos.x - 1 || tailPos.x == headPos.x - 2)
                return Point(tailPos.x + 1, tailPos.y + 1)
        if (tailPos.x == headPos.x + 1 || tailPos.x == headPos.x + 2)
                return Point(tailPos.x - 1, tailPos.y + 1)
        return Point(tailPos.x, tailPos.y + 1)
    } else if (tailPos.y == headPos.y + 2) {
        if (tailPos.x == headPos.x - 1 || tailPos.x == headPos.x - 2)
                return Point(tailPos.x + 1, tailPos.y - 1)
        if (tailPos.x == headPos.x + 1 || tailPos.x == headPos.x + 2)
                return Point(tailPos.x - 1, tailPos.y - 1)
        return Point(tailPos.x, tailPos.y - 1)
    }
    return tailPos
}
