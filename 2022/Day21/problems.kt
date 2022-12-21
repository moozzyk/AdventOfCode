import java.io.File

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val regex = """(.*): (.*)""".toRegex()
    val monkeys =
            lines
                    .map { regex.matchEntire(it)!!.destructured }
                    .map { (name, operation) -> name to operation }
                    .toMap()
    println(problem1(monkeys))
    println(problem2(monkeys))
}

fun problem1(monkeys: Map<String, String>): Long {
    return computeValue("root", monkeys)
}

fun problem2(monkeys: Map<String, String>): Long {
    val (m1, m2) = "(.+) . (.+)".toRegex().matchEntire(monkeys["root"]!!)!!.destructured
    if (hasHumn(m1, monkeys)) {
        val target = computeValue(m2, monkeys)
        return findHumnValue(m1, target, monkeys)
    } else {
        val target = computeValue(m1, monkeys)
        return findHumnValue(m2, target, monkeys)
    }
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

fun hasHumn(monkey: String, monkeys: Map<String, String>): Boolean {
    if (monkey == "humn") {
        return true
    }
    val regex = "(.+) . (.+)".toRegex()
    val operation = monkeys[monkey]
    var matches = regex.matchEntire(operation!!)
    if (matches == null) {
        return false
    }
    val (m1, m2) = matches.destructured
    return hasHumn(m1, monkeys) || hasHumn(m2, monkeys)
}

fun findHumnValue(monkey: String, target: Long, monkeys: Map<String, String>): Long {
    if (monkey == "humn") {
        return target
    }

    val (m1, operation, m2) =
            "(.+) (.) (.+)".toRegex().matchEntire(monkeys[monkey]!!)!!.destructured
    if (hasHumn(m1, monkeys)) {
        val value = computeValue(m2, monkeys)
        val newTarget = calculateNewTarget(target, operation, value, isLeft = true)
        return findHumnValue(m1, newTarget, monkeys)
    } else {
        val value = computeValue(m1, monkeys)
        var newTarget = calculateNewTarget(target, operation, value, isLeft = false)
        return findHumnValue(m2, newTarget, monkeys)
    }
}

fun calculateNewTarget(target: Long, operation: String, knownOperand: Long, isLeft: Boolean): Long {
    return when (operation) {
        "+" -> target - knownOperand
        "-" ->
                if (isLeft) {
                    target + knownOperand
                } else {
                    -(target - knownOperand)
                }
        "*" -> target / knownOperand
        "/" ->
                if (isLeft) {
                    target * knownOperand
                } else {
                    knownOperand / target
                }
        else -> throw Exception("Invalid operation")
    }
}
