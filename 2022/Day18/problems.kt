import java.io.File
import kotlin.math.*

typealias Position = Triple<Int, Int, Int>

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val cubes =
            lines.map { it.split(",").map { it.toInt() }.toList() }.map {
                Position(it[0], it[1], it[2])
            }
    println(problem1(cubes))
    println(problem2(cubes))
}

fun problem1(cubes: List<Position>): Int {
    var seen = mutableSetOf<Position>()
    var result = 0
    for (c in cubes) {
        val numNeighbors = generateNeighbors(c).map { seen.contains(it) }.filter { it }.count()
        seen.add(c)
        result -= numNeighbors
        result += 6 - numNeighbors
    }
    return result
}

fun problem2(cubes: List<Position>): Int {
    var seen = cubes.toMutableSet()
    seen.add(Position(0, 0, 0))
    fill(Position(0, 0, 0), seen)

    var subtract = 0
    for (x in 0..24) {
        for (y in 0..24) {
            for (z in 0..24) {
                val cube = Position(x, y, z)
                if (!seen.contains(cube)) {
                    subtract +=
                            generateNeighbors(cube).map { seen.contains(it) }.filter { it }.count()
                }
            }
        }
    }
    return problem1(cubes) - subtract
}

fun fill(cube: Position, seen: MutableSet<Position>) {
    var q = ArrayDeque<Position>()
    q.add(cube)
    while (!q.isEmpty()) {
        val c = q.removeFirst()
        for (n in generateNeighbors(c)) {
            val (x, y, z) = n
            if (x < 0 || y < 0 || z < 0 || x > 24 || y > 24 || z > 24) {
                continue
            }
            if (!seen.contains(n)) {
                seen.add(n)
                q.addLast(n)
            }
        }
    }
}

fun generateNeighbors(cube: Position): List<Position> {
    val (x, y, z) = cube
    return listOf(
            Position(x - 1, y, z),
            Position(x + 1, y, z),
            Position(x, y - 1, z),
            Position(x, y + 1, z),
            Position(x, y, z - 1),
            Position(x, y, z + 1)
    )
}
