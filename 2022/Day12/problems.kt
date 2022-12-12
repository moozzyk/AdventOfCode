import java.io.File
import java.util.ArrayDeque
import kotlin.text.toCharArray

data class Position(val row: Int, val col: Int) {}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(problem1(lines.map { it.toCharArray() }))
    println(problem2(lines.map { it.toCharArray() }))
}

fun problem1(map: List<CharArray>): Int {
    val start = findPosition('S', map)
    val end = findPosition('E', map)
    map[start.row][start.col] = 'a'
    map[end.row][end.col] = 'z'
    return visit(listOf(start), end, map)
}

fun problem2(map: List<CharArray>): Int {
    val start = findPosition('S', map)
    val end = findPosition('E', map)
    map[start.row][start.col] = 'a'
    map[end.row][end.col] = 'z'

    var startPos = mutableListOf<Position>()
    for (r in 0 until map.size) {
        for (c in 0 until map[0].size) {
            if (map[r][c] == 'a') {
                startPos.add(Position(r, c))
            }
        }
    }

    return visit(startPos, end, map)
}

fun visit(startPos: List<Position>, endPos: Position, map: List<CharArray>): Int {
    var visited = mutableSetOf<Position>()
    var queue = ArrayDeque<Pair<Position, Int>>()
    startPos.forEach {
        queue.addLast(Pair(it, 0))
        visited.add(it)
    }
    while (!queue.isEmpty()) {
        val (position, steps) = queue.removeFirst()
        if (position == endPos) {
            return steps
        }
        visited.add(position)

        val (r, c) = position
        if (isValid(r, c - 1, map[r][c], map) && !visited.contains(Position(r, c - 1))) {
            queue.addLast(Pair(Position(r, c - 1), steps + 1))
            visited.add(Position(r, c - 1))
        }
        if (isValid(r, c + 1, map[r][c], map) && !visited.contains(Position(r, c + 1))) {
            queue.addLast(Pair(Position(r, c + 1), steps + 1))
            visited.add(Position(r, c + 1))
        }
        if (isValid(r - 1, c, map[r][c], map) && !visited.contains(Position(r - 1, c))) {
            queue.addLast(Pair(Position(r - 1, c), steps + 1))
            visited.add(Position(r - 1, c))
        }
        if (isValid(r + 1, c, map[r][c], map) && !visited.contains(Position(r + 1, c))) {
            queue.addLast(Pair(Position(r + 1, c), steps + 1))
            visited.add(Position(r + 1, c))
        }
    }
    return -1
}

fun findPosition(x: Char, map: List<CharArray>): Position {
    for (r in 0 until map.size) {
        for (c in 0 until map[0].size) {
            if (map[r][c] == x) {
                return Position(r, c)
            }
        }
    }
    throw Exception("Not found.")
}

fun isValid(row: Int, col: Int, current: Char, map: List<CharArray>): Boolean =
        isInMap(row, col, map) && canMoveTo(row, col, current, map)

fun isInMap(row: Int, col: Int, map: List<CharArray>): Boolean =
        row >= 0 && col >= 0 && row < map.size && col < map[0].size

fun canMoveTo(row: Int, col: Int, current: Char, map: List<CharArray>): Boolean =
        map[row][col].code - current.code <= 1
