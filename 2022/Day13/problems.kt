import java.io.File
import kotlin.math.min

fun main(args: Array<String>) {

    val lines = File(args[0]).readLines()
    println(problem1(lines))
    println(problem2(lines))
}

fun problem1(lines: List<String>): Int {
    val results = lines.windowed(3, 3).map { runCompare(it[0], it[1]) }
    return results
            .mapIndexed { idx, v ->
                if (v) {
                    idx + 1
                } else {
                    0
                }
            }
            .sum()
}

fun problem2(lines: List<String>): Int {
    val divider1 = "[[2]]"
    val divider2 = "[[6]]"
    val newLines = lines.filter { it != "" }.toMutableList()
    newLines.add(divider1)
    newLines.add(divider2)
    newLines.sortWith(
            Comparator<String> { a, b ->
                val result = compare(a, b)
                when (result) {
                    Result.GOOD -> -1
                    Result.BAD -> 1
                    Result.CONTINUE -> 0
                }
            }
    )

    return (newLines.indexOf(divider1) + 1) * (newLines.indexOf(divider2) + 1)
}

enum class Result {
    GOOD,
    BAD,
    CONTINUE
}

fun runCompare(s1: String, s2: String): Boolean {
    val result = compare(s1, s2)
    return result != Result.BAD
}

fun compare(s1: String, s2: String): Result {
    if (s1[0] == '[' && s2[0] == '[') {
        return compareLists(s1, s2)
    }

    if (s1[0].isDigit() && s2[0].isDigit()) {
        return compareScalars(s1, s2)
    }
    return compareMixed(s1, s2)
}

fun compareLists(s1: String, s2: String): Result {
    val items1 = getListItems(s1)
    val items2 = getListItems(s2)

    for (i in 0 until min(items1.size, items2.size)) {
        val res = compare(items1[i], items2[i])
        if (res != Result.CONTINUE) {
            return res
        }
    }

    return when {
        items1.size < items2.size -> Result.GOOD
        items1.size > items2.size -> Result.BAD
        else -> Result.CONTINUE
    }
}

fun compareScalars(s1: String, s2: String): Result {
    val n1 = s1.toInt()
    val n2 = s2.toInt()
    return when {
        n1 < n2 -> Result.GOOD
        n1 > n2 -> Result.BAD
        else -> Result.CONTINUE
    }
}

fun compareMixed(s1: String, s2: String): Result {
    if (s1[0].isDigit()) {
        return compareLists("[${s1}]", s2)
    }
    return compareLists(s1, "[${s2}]")
}

fun getListItems(s: String): List<String> {
    var i = 1
    var items = mutableListOf<String>()
    while (i < s.length - 1) {
        if (s[i].isDigit()) {
            var numLen = extractNum(s, i)
            items.add(s.substring(i, i + numLen))
            i += numLen
        } else {
            var sublistLen = extractList(s, i)
            items.add(s.substring(i, i + sublistLen))
            i += sublistLen
        }
        i++ // skip ',' or ']'
    }
    return items
}

fun extractList(s: String, startIndex: Int): Int {
    var numParens = 0
    var i = startIndex
    while (i < s.length) {
        if (s[i] == '[') {
            numParens++
        } else if (s[i] == ']') {
            if (--numParens == 0) {
                return i + 1 - startIndex
            }
        }
        i++
    }
    return -1
}

fun extractNum(s: String, _i: Int): Int {
    var i = _i
    while (s[i].isDigit()) {
        i++
    }
    return i - _i
}

fun compare1(s1: String, s2: String): Boolean {
    var i = 0
    var j = 0
    while (i < s1.length && j < s2.length) {
        println("$i $j")
        if (s1[i] == '[' && s2[j] == '[') {
            i++
            j++
        } else if (s1[i] == '[' && s2[j].isDigit()) {
            i++
        } else if (s1[i].isDigit() && s2[j] == '[') {
            j++
        } else if (s1[i] == ']' && s2[j] == ']') {
            i++
            j++
        } else if (s1[i] == ',' && s2[j] == ',') {
            i++
            j++
        } else if (s1[i].isDigit() && s2[j].isDigit()) {
            val n1Len = numLength(s1, i)
            val n2Len = numLength(s2, j)
            val n1 = s1.substring(i, i + n1Len).toInt()
            val n2 = s2.substring(j, j + n2Len).toInt()
            i += n1Len
            j += n2Len
            if (n1 < n2) return true
            if (n1 > n2) return false
            if (s1[i] == ']' && s2[j] != ']') return true
            if (s1[i] != ']' && s2[j] == ']') return false
            // both are ',' or both are ']'
            i++
            j++
        } else if (s1[i] == ']' && s2[j] != ']') {
            return true
        } else if (s1[i] != ']' && s2[j] == ']') {
            return false
        } else {
            throw Exception("Unexpected state: ${i} ${j}")
        }
    }

    return true
}

fun numLength(s: String, _i: Int): Int {
    var i = _i
    while (s[i].isDigit()) {
        i++
    }
    return i - _i
}
