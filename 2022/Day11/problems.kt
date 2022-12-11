import java.io.File
import java.util.ArrayDeque

class Monkey(
        var items: ArrayDeque<Int>,
        val operation: Char,
        val operand: String,
        val divisor: Int,
        val targetIfTrue: Int,
        val targetIfFalse: Int
) {
    var timesInspected = 0

    fun getItem(): Int {
        timesInspected++
        return items.removeFirst()
    }

    fun computeNewWorryLevel(oldVal: Int): Int {
        val value =
                if (operand == "old") {
                    oldVal
                } else {
                    operand.toInt()
                }

        return if (operation == '*') {
            oldVal * value
        } else {
            oldVal + value
        }
    }

    fun nextMonkey(worryLevel: Int): Int {
        return if (worryLevel % divisor == 0) {
            targetIfTrue
        } else {
            targetIfFalse
        }
    }
}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val monkeys = lines.windowed(7, 7).map { parse(it) }
    println(problem1(monkeys))
}

fun parse(lines: List<String>): Monkey {
    val regex = """(\d+)""".toRegex()
    val items = regex.findAll(lines[1]).map { it.value.toInt() }
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

fun problem1(monkeys: List<Monkey>): Int {
    for (i in 1..20) {
        for (m in monkeys) {
            while (!m.items.isEmpty()) {
                var worryLevel = m.getItem()
                worryLevel = m.computeNewWorryLevel(worryLevel) / 3
                val targetMonkey = m.nextMonkey(worryLevel)
                monkeys[targetMonkey].items.addLast(worryLevel)
            }
        }
        // println("round: ${i}")
        // monkeys.forEach { println(it.items.toList()) }
    }

    var topInspected = monkeys.map { it.timesInspected }.sortedDescending().take(2)
    return topInspected.reduce { acc, el -> acc * el }
}
