import java.io.File
import kotlin.text.*

data class Segment(val from: Int, val to: Int) {}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val regex = Regex("\\d+")
    val segments =
            lines.map { regex.findAll(it).toList() }.map {
                Pair(
                        Segment(it[0].value.toInt(), it[1].value.toInt()),
                        Segment(it[2].value.toInt(), it[3].value.toInt())
                )
            }
    println(problem1(segments))
    println(problem2(segments))
}

fun contains(s1: Segment, s2: Segment): Boolean {
    return s1.from <= s2.from && s1.to >= s2.to
}

fun overlaps(s1: Segment, s2: Segment): Boolean {
    return !(s2.to < s1.from || s2.from > s1.to)
}

fun problem1(segments: List<Pair<Segment, Segment>>): Int {
    return problem(segments, { s1, s2 -> s1.from <= s2.from && s1.to >= s2.to })
}

fun problem2(segments: List<Pair<Segment, Segment>>): Int {
    return problem(segments, ::overlaps)
}

fun problem(
        segments: List<Pair<Segment, Segment>>,
        operation: (s1: Segment, s2: Segment) -> Boolean
): Int {
    return segments
            .map { operation.invoke(it.first, it.second) || operation.invoke(it.second, it.first) }
            .filter { it }
            .count()
}
