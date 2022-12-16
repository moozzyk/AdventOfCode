import java.io.File

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val cave = buildCave(lines)
    println(problem1(cave))
}

fun problem1(cave: Array<Array<Char>>): Int {
    var numGrains = 0
    while (!dropSand(cave)) {
        numGrains++
    }
    return numGrains
}

fun buildCave(lines: List<String>): Array<Array<Char>> {
    var cave = Array<Array<Char>>(200) { Array<Char>(1000) { ' ' } }
    lines.forEach {
        it.split(" -> ").windowed(2).forEach {
            var (fromCol, fromRow) = it[0].split(",").map { it.toInt() }
            val (toCol, toRow) = it[1].split(",").map { it.toInt() }
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
    return cave
}

fun dropSand(cave: Array<Array<Char>>): Boolean {
    var row = 0
    var col = 500

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
