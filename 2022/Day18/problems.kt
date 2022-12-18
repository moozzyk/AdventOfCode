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
