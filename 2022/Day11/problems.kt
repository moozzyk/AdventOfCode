import java.io.File
import java.util.ArrayDeque

class Monkey(
        var items: ArrayDeque<Long>,
        val operation: Char,
        val operand: String,
        val divisor: Int,
        val targetIfTrue: Int,
        val targetIfFalse: Int
) {
    var timesInspected = 0

    fun getItem(): Long {
        timesInspected++
        return items.removeFirst()
    }

    fun computeNewWorryLevel(oldVal: Long): Long {
        val value =
                if (operand == "old") {
                    oldVal
                } else {
                    operand.toLong()
                }

        return if (operation == '*') {
            oldVal * value
        } else {
            oldVal + value
        }
    }

    fun nextMonkey(worryLevel: Long): Int {
        return if (worryLevel % divisor == 0L) {
            targetIfTrue
        } else {
            targetIfFalse
        }
    }
}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(problem1(lines.windowed(7, 7).map { parse(it) }))
    println(problem2(lines.windowed(7, 7).map { parse(it) }))
}

fun parse(lines: List<String>): Monkey {
    val regex = """(\d+)""".toRegex()
    val items = regex.findAll(lines[1]).map { it.value.toLong() }
    val (operation, operand) = """old (.) (old|\d+)""".toRegex().find(lines[2])!!.destructured
    val (divisor) = regex.find(lines[3])!!.destructured
    val (targetIfTrue) = regex.find(lines[4])!!.destructured
    val (targetIfFalse) = regex.find(lines[5])!!.destructured
    return Monkey(
            ArrayDeque(items.toList()),
            operation.first(),
            operand,
            divisor.toInt(),
            targetIfTrue.toInt(),
            targetIfFalse.toInt()
    )
}

fun solve(monkeys: List<Monkey>, rounds: Int, reductionFactor: Int): Long {
    val mod = monkeys.map { it.divisor }.reduce { a, e -> a * e }
    for (i in 1..rounds) {
        for (m in monkeys) {
            while (!m.items.isEmpty()) {
                var worryLevel = m.getItem()
                worryLevel = (m.computeNewWorryLevel(worryLevel) / reductionFactor) % mod
                val targetMonkey = m.nextMonkey(worryLevel)
                monkeys[targetMonkey].items.addLast(worryLevel)
            }
        }
    }

    println(monkeys.map { it.timesInspected.toLong() })
    var topInspected = monkeys.map { it.timesInspected.toLong() }.sortedDescending().take(2)
    return topInspected.reduce { acc, el -> acc * el }
}

fun problem1(monkeys: List<Monkey>): Long = solve(monkeys, 20, 3)

fun problem2(monkeys: List<Monkey>): Long = solve(monkeys, 10000, 1)
