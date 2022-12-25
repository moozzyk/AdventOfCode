import java.io.File
import kotlin.math.*

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(lines.reduce() { acc, n -> add(acc, n) })
}

fun add(s1: String, s2: String): String {
    val maxLen = max(s1.length, s2.length)
    var a1 = s1.padStart(maxLen, '0').reversed().map { SnafuDigitToNumber(it) }.toList()
    var a2 = s2.padStart(maxLen, '0').reversed().map { SnafuDigitToNumber(it) }.toList()

    val result = mutableListOf<Int>()
    var carry = 0
    for (i in 0 until a1.size) {
        var tmp = carry + a1[i] + a2[i]
        carry = 0
        if (tmp > 2) {
            carry = 1
            tmp -= 5
        } else if (tmp < -2) {
            tmp += 5
            carry = -1
        }
        result.add(tmp)
    }
    if (carry != 0) {
        result.add(carry)
    }

    return result.map { NumberToSnafuDigit(it) }.reversed().joinToString("")
}

fun SnafuToLong(snafu: String): Long {
    return snafu.map { SnafuDigitToNumber(it).toLong() }.reduce { acc, n -> acc * 5 + n }
}

fun SnafuDigitToNumber(c: Char): Int =
        when (c) {
            '=' -> -2
            '-' -> -1
            '0' -> 0
            '1' -> 1
            '2' -> 2
            else -> throw Exception("Unexpected SNAFU digit.")
        }

fun NumberToSnafuDigit(n: Int): Char =
        when (n) {
            -2 -> '='
            -1 -> '-'
            0 -> '0'
            1 -> '1'
            2 -> '2'
            else -> throw Exception("Out of range.")
        }
