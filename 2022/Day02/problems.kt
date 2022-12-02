import java.io.File
import kotlin.math.*

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(problem1(lines))
}

fun problem1(lines: List<String>): Int {
    val results =
            lines.map {
                when (it) {
                    "A X" -> 4
                    "A Y" -> 8
                    "A Z" -> 3
                    "B X" -> 1
                    "B Y" -> 5
                    "B Z" -> 9
                    "C X" -> 7
                    "C Y" -> 2
                    "C Z" -> 6
                    else -> 0
                }
            }
    return results.sum()
}
