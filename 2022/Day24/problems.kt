import java.io.File
import java.util.ArrayDeque
import kotlin.math.*

val EMPTY = 0
val WALL = 1
val UP = 2
val DOWN = 4
val LEFT = 8
val RIGHT = 16

data class State(val row: Int, val col: Int, val ticks: Int)

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(problem1(lines))
    println(problem2(lines))
}

fun problem1(lines: List<String>): Int {
    var areas = mutableListOf(createArea(lines))
    return findPath(areas, Pair(0, 1), Pair(lines.size - 1, lines[0].length - 2))
}

fun problem2(lines: List<String>): Int {
    var areas = mutableListOf(createArea(lines))
    val trip1 = findPath(areas, Pair(0, 1), Pair(lines.size - 1, lines[0].length - 2))
    areas = mutableListOf(areas[trip1])
    val trip2 = findPath(areas, Pair(lines.size - 1, lines[0].length - 2), Pair(0, 1))
    areas = mutableListOf(areas[trip2])
    val trip3 = findPath(areas, Pair(0, 1), Pair(lines.size - 1, lines[0].length - 2))
    return trip1 + trip2 + trip3
}

fun findPath(
        areas: MutableList<List<MutableList<Int>>>,
        start: Pair<Int, Int>,
        end: Pair<Int, Int>
): Int {
    var q = ArrayDeque<State>()
    q.add(State(start.first, start.second, 0))
    var visited = mutableSetOf(q.first())
    while (!q.isEmpty()) {
        var (row, col, ticks) = q.removeFirst()
        if (row == end.first && col == end.second) {
            return ticks - 1
        }
        if (areas.size == ticks) {
            areas.add(tick(areas.last()))
        }
        val area = areas[ticks]
        for (newPos in
                listOf(
                        Pair(row, col),
                        Pair(row - 1, col),
                        Pair(row + 1, col),
                        Pair(row, col - 1),
                        Pair(row, col + 1)
                )) {
            val (r, c) = newPos
            if (r >= 0 && r < area.size && area[r][c] == EMPTY) {
                val newState = State(r, c, ticks + 1)
                if (!visited.contains(newState)) {
                    visited.add(newState)
                    q.addLast(newState)
                }
            }
        }
    }
    throw Exception("No solutions found")
}

fun createArea(lines: List<String>): List<MutableList<Int>> {
    return lines.map {
        it.toCharArray()
                .map {
                    when (it) {
                        '.' -> EMPTY
                        '#' -> WALL
                        '^' -> UP
                        'v' -> DOWN
                        '<' -> LEFT
                        '>' -> RIGHT
                        else -> throw Exception("Unexpected input")
                    }
                }
                .toMutableList()
    }
}

fun tick(area: List<MutableList<Int>>): List<MutableList<Int>> {
    var newArea = area.map { it.map { if (it == WALL) WALL else EMPTY }.toMutableList() }.toList()

    for (row in 0 until area.size) {
        for (col in 0 until area[row].size) {
            if ((area[row][col] and RIGHT) != 0) {
                val c = if (area[row][col + 1] == WALL) 1 else col + 1
                newArea[row][c] = newArea[row][c] or RIGHT
            }
            if ((area[row][col] and LEFT) != 0) {
                val c = if (area[row][col - 1] == WALL) area[row].size - 2 else col - 1
                newArea[row][c] = newArea[row][c] or LEFT
            }
            if ((area[row][col] and UP) != 0) {
                val r = if (area[row - 1][col] == WALL) area.size - 2 else row - 1
                newArea[r][col] = newArea[r][col] or UP
            }
            if ((area[row][col] and DOWN) != 0) {
                val r = if (area[row + 1][col] == WALL) 1 else row + 1
                newArea[r][col] = newArea[r][col] or DOWN
            }
        }
    }

    return newArea
}

fun printArea(area: List<MutableList<Int>>) {
    for (row in area) {
        for (loc in row) {
            print(
                    when (loc) {
                        EMPTY -> ' '
                        WALL -> '#'
                        UP -> '^'
                        DOWN -> 'v'
                        RIGHT -> '>'
                        LEFT -> '<'
                        else -> '*'
                    }
            )
        }
        println()
    }
    println()
}
