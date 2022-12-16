import java.io.File
import kotlin.math.max

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(problem1(lines))
    println(problem2(lines))
}

fun problem1(lines: List<String>): Int {
    val cave = buildCave(lines, addFloor = false)
    var numGrains = 0
    while (!dropSand(cave)) {
        numGrains++
    }
    return numGrains
}

fun problem2(lines: List<String>): Int {
    val cave = buildCave(lines, addFloor = true)
    var numGrains = 0
    while (!dropSand(cave)) {
        numGrains++
    }
    return numGrains
}

fun buildCave(lines: List<String>, addFloor: Boolean): Array<Array<Char>> {
    var cave = Array<Array<Char>>(200) { Array<Char>(1000) { ' ' } }
    var maxDepth = 0
    lines.forEach {
        it.split(" -> ").windowed(2).forEach {
            var (fromCol, fromRow) = it[0].split(",").map { it.toInt() }
            val (toCol, toRow) = it[1].split(",").map { it.toInt() }
            maxDepth = max(max(maxDepth, fromRow), toRow)
            while (fromCol != toCol || fromRow != toRow) {
                cave[fromRow][fromCol] = '#'
                if (fromCol < toCol) {
                    fromCol++
                } else if (fromCol > toCol) {
                    fromCol--
                } else if (fromRow < toRow) {
                    fromRow++
                } else if (fromRow > toRow) {
                    fromRow--
                }
            }
            cave[fromRow][fromCol] = '#'
        }
    }
    if (addFloor) {
        cave[maxDepth + 2].fill('#')
    }
    return cave
}

fun dropSand(cave: Array<Array<Char>>): Boolean {
    var row = 0
    var col = 500

    if (cave[row][col] != ' ') {
        return true
    }

    while (row < cave.size - 1) {
        if (cave[row + 1][col] == ' ') {
            row++
        } else if (cave[row + 1][col - 1] == ' ') {
            row++
            col--
        } else if (cave[row + 1][col + 1] == ' ') {
            row++
            col++
        } else {
            cave[row][col] = 'o'
            return false
        }
    }

    cave[row][col] = 'o'
    return true
}
