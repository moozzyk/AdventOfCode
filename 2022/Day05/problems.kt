import java.io.File
import java.util.ArrayDeque
import kotlin.collections.indexOf

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val separatorIdx = lines.indexOf("")
    val stackLines = lines.subList(0, separatorIdx)
    val moveLines = lines.subList(separatorIdx + 1, lines.size)
    println(problem1(createStacks(stackLines), moveLines))
    println(problem2(createStacks(stackLines), moveLines))
}

fun createStacks(stackLines: List<String>): List<ArrayDeque<Char>> {
    val numStacks = stackLines.last().takeLast(2).first().toString().toInt()
    val stacks = List(numStacks, { ArrayDeque<Char>() })
    for (i in stackLines.size - 2 downTo 0) {
        for (j in 0..numStacks - 1) {
            val crate = stackLines[i][1 + j * 4]
            if (crate != ' ') {
                stacks[j].push(crate)
            }
        }
    }
    return stacks
}

fun problem1(stacks: List<ArrayDeque<Char>>, moves: List<String>): String {
    val regex = """move (\d+) from (\d+) to (\d+)""".toRegex()
    for (move in moves) {
        val (count, from, to) = regex.find(move)!!.destructured
        for (i in 1..count.toInt()) {
            stacks[to.toInt() - 1].push(stacks[from.toInt() - 1].pop())
        }
    }
    return stacks.map { it.peek() }.joinToString("")
}

fun problem2(stacks: List<ArrayDeque<Char>>, moves: List<String>): String {
    val regex = """move (\d+) from (\d+) to (\d+)""".toRegex()
    for (move in moves) {
        val (count, from, to) = regex.find(move)!!.destructured
        val buffer = mutableListOf<Char>()
        for (i in 1..count.toInt()) {
            buffer.add(stacks[from.toInt() - 1].pop())
        }

        for (crate in buffer.asReversed()) {
            stacks[to.toInt() - 1].push(crate)
        }
    }
    return stacks.map { it.peek() }.joinToString("")
}
