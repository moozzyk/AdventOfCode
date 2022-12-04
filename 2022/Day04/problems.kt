import java.io.File
import kotlin.text.*

data class Segment(val from: Int, val to: Int) {}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val regex = Regex("""(\d+)-(\d+),(\d+)-(\d+)""")
    val segments =
            lines.map { regex.matchEntire(it)!!.destructured }.map { (s1from, s1to, s2from, s2to) ->
                Pair(Segment(s1from.toInt(), s1to.toInt()), Segment(s2from.toInt(), s2to.toInt()))
            }
    println(problem1(segments))
    println(problem2(segments))
}

fun contains(s1: Segment, s2: Segment): Boolean = s1.from <= s2.from && s1.to >= s2.to

fun overlaps(s1: Segment, s2: Segment): Boolean = !(s2.to < s1.from || s2.from > s1.to)

fun problem1(segments: List<Pair<Segment, Segment>>): Int = problem(segments, ::contains)

fun problem2(segments: List<Pair<Segment, Segment>>): Int = problem(segments, ::overlaps)

fun problem(
        segments: List<Pair<Segment, Segment>>,
        operation: (s1: Segment, s2: Segment) -> Boolean
): Int =
        segments.map { operation(it.first, it.second) || operation(it.second, it.first) }.count {
            it
        }
