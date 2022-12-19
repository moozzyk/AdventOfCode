import java.io.File
import kotlin.math.*

class Cave(val winds: String) {
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

    fun run(numRocks: Long) {
        for (rockNo in 0 until numRocks) {
            val rock = rocks[(rockNo % 5).toInt()]
            expandCave(rock)
            dropRock(rock)
        }
    }

    fun findCycle(): Triple<Int, Int, Int> {
        var cycleMap = mutableMapOf<Triple<Int, Int, String?>, Pair<Int, Int>>()
        var rockNo = 0
        while (true) {
            val rock = rocks[(rockNo % 5).toInt()]
            trim()
            val key = Triple(windsIdx, (rockNo % 5).toInt(), cave.firstOrNull()?.joinToString(""))
            if (!cycleMap.containsKey(key)) {
                cycleMap[key] = Pair(rockNo, height())
            } else {
                val (cycleStart, caveHeight) = cycleMap[key]!!
                val cycleLength = rockNo - cycleStart
                val cycleCaveHeight = height() - caveHeight
                return Triple(cycleStart, cycleLength, cycleCaveHeight)
                // cycleMap[key]!!.add(rockNo)
            }
            expandCave(rock)
            dropRock(rock)
            rockNo++
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
    println(problem2(winds))
}

fun problem1(winds: String): Long {
    return solve(winds, 2022)
}

fun problem2(winds: String): Long {
    return solve(winds, 1000000000000)
}

fun solve(winds: String, steps: Long): Long {
    var cave = Cave(winds)
    val (cycleStart, cycleLength, cycleCaveHeight) = cave.findCycle()

    cave = Cave(winds)
    val stepsToRun = cycleStart + (steps - cycleStart) % cycleLength
    cave.run(stepsToRun)
    var height = cave.height()
    var skippedHeight = ((steps - stepsToRun) / cycleLength) * cycleCaveHeight
    return height + skippedHeight
}
