import java.io.File
import kotlin.jvm.internal.iterator

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(problem1(lines))
    println(problem2(lines))
}

fun problem1(lines: List<String>): Int {
    return lines.map { findDuplicate(it) }.map { getPriority(it) }.sum()
}

fun problem2(lines: List<String>): Int {
    val badges = lines.windowed(3, 3).map { findCommon(it.map { it.toCharArray().toSet() }) }
    return badges.map { getPriority(it.first()) }.sum()
}

fun findDuplicate(line: String): Char {
    val strings = listOf(line.substring(0, line.length / 2), line.substring(line.length / 2))
    return findCommon(strings.map { it.toCharArray().toSet() }).first()
}

fun findCommon(sets: List<Set<Char>>): Set<Char> {
    var common = sets.first().toMutableSet()
    for (s in sets) {
        common.retainAll(s)
    }
    return common
}

fun getPriority(item: Char): Int {
    if (item.isLowerCase()) {
        return item.code - 'a'.code + 1
    }

    return item.code - 'A'.code + 27
}
