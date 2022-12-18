import java.io.File
import kotlin.math.*

class CaveNew(val winds: String) {
    val rocks =
            listOf(
                    listOf("@@@@"),
                    listOf(" @ ", "@@@", " @ "),
                    listOf("  @", "  @", "@@@"),
                    listOf("@", "@", "@", "@"),
                    listOf("@@", "@@")
            )

    var cave = ArrayDeque<CharArray>()
    var windsIdx = 0

    fun print() {
        cave.forEach { println("|${it.joinToString("")}|") }
        println("---------\n")
    }

    fun print(rock: List<String>, row: Int, col: Int) {
        var rows = cave.map { it.toMutableList() }
        for (r in 0 until rock.size) {
            for (c in 0 until rock[0].length) {
                if (rock[r][c] == '@') {
                    rows[row + r][col + c] = '@'
                }
            }
        }

        rows.forEach { println("|${it.joinToString("")}|") }
        println("---------\n")
    }

    fun run() {
        for (rockNo in 0 until 2022) {
            val rock = rocks[rockNo % 5]
            expandCave(rock)
            dropRock(rock)
        }
    }

    fun expandCave(rock: List<String>) {
        trim()
        for (x in 0 until 3 + rock.size) {
            cave.addFirst(CharArray(7) { ' ' })
        }
    }

    fun trim() {
        while (cave.size > 0 && cave.first().all { it == ' ' }) {
            cave.removeFirst()
        }
    }

    fun dropRock(rock: List<String>) {
        var row = 0
        var col = 2
        while (true) {
            // this.print(rock, row, col)
            var windDir = winds[windsIdx]
            if (windDir == '>' && isValidPosition(rock, row, col + 1)) {
                col++
            }
            if (windDir == '<' && isValidPosition(rock, row, col - 1)) {
                col--
            }
            windsIdx = (windsIdx + 1) % winds.length
            if (isValidPosition(rock, row + 1, col)) {
                row++
            } else {
                for (r in 0 until rock.size) {
                    for (c in 0 until rock[0].length) {
                        if (rock[r][c] == '@') {
                            cave[row + r][col + c] = '#'
                        }
                    }
                }
                // this.print()
                return
            }
        }
    }

    fun isValidPosition(rock: List<String>, row: Int, col: Int): Boolean {
        if (col < 0 || col + rock[0].length > cave[0].size || row + rock.size > cave.size) {
            return false
        }

        for (r in 0 until rock.size) {
            for (c in 0 until rock[0].length) {
                if (rock[r][c] == '@' && cave[row + r][col + c] == '#') {
                    return false
                }
            }
        }
        return true
    }

    fun height(): Int {
        trim()
        return cave.size
    }
}

fun main(args: Array<String>) {
    val winds = File(args[0]).readText()
    println(problem1(winds))
}

fun problem1(winds: String): Int {
    var cave = CaveNew(winds)
    cave.run()
    return cave.height()
}
