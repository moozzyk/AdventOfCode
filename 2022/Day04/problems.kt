import java.io.File
import kotlin.text.*

data class Segment(val from: Int, val to: Int) {}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(problem1(lines))
}

fun problem1(lines: List<String>): Int {
    val regex = Regex("\\d+")
    val segments =
            lines.map { regex.findAll(it).toList() }.map {
                Pair(
                        Segment(it[0].value.toInt(), it[1].value.toInt()),
                        Segment(it[2].value.toInt(), it[3].value.toInt())
                )
            }
    return segments
            .map {
                (it.first.from <= it.second.from && it.first.to >= it.second.to) ||
                        (it.second.from <= it.first.from && it.second.to >= it.first.to)
            }
            .filter { it }
            .count()
}
