import java.io.File
import kotlin.jvm.internal.iterator

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(problem1(lines))
}

fun problem1(lines: List<String>): Int {
    return lines.map { findDuplicate(it) }.map { getPriority(it) }.sum()
}

fun findDuplicate(line: String): Char {
    return line.substring(0, line.length / 2)
            .toCharArray()
            .intersect(line.substring(line.length / 2).toCharArray().toSet())
            .first()
}

fun getPriority(item: Char): Int {
    if (item.isLowerCase()) {
        return item.code - 'a'.code + 1
    }

    return item.code - 'A'.code + 27
}
