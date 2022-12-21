import java.io.File
import kotlin.math.*

data class Monkey(val name: String, val operation: String)

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val regex = """(.*): (.*)""".toRegex()
    val monkeys =
            lines
                    .map { regex.matchEntire(it)!!.destructured }
                    .map { (name, operation) -> name to operation }
                    .toMap()

    println(problem1(monkeys))
}

fun problem1(monkeys: Map<String, String>): Long {
    return computeValue("root", monkeys)
}

fun computeValue(monkey: String, monkeys: Map<String, String>): Long {
    val regex = "(.+) (.) (.+)".toRegex()
    val operation = monkeys[monkey]
    var matches = regex.matchEntire(operation!!)
    if (matches == null) {
        return operation.toLong()
    }
    val (m1, op, m2) = matches.destructured
    return when (op) {
        "-" -> computeValue(m1, monkeys) - computeValue(m2, monkeys)
        "+" -> computeValue(m1, monkeys) + computeValue(m2, monkeys)
        "*" -> computeValue(m1, monkeys) * computeValue(m2, monkeys)
        "/" -> computeValue(m1, monkeys) / computeValue(m2, monkeys)
        else -> throw Exception("Invalid operator")
    }
}
