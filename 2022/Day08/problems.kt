import java.io.File

data class Position(val row: Int, val col: Int) {}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val forest = lines.map { it.toCharArray().map { it.code - '0'.code } }
    println(problem1(forest))
}

fun problem1(forest: List<List<Int>>): Int {
    var numVisible = 0
    for (row in 0..forest[0].size - 1) {
        for (col in 0..forest[0].size - 1) {
            if (isVisible(forest, row, col)) numVisible++
        }
    }
    return numVisible
}

fun isVisible(forest: List<List<Int>>, row: Int, col: Int): Boolean {
    val pos = Position(row, col)
    return isVisibleFromRight(forest, pos) ||
            isVisibleFromLeft(forest, pos) ||
            isVisibleFromTop(forest, pos) ||
            isVisibleFromBottom(forest, pos)
}

fun isVisibleFromRight(forest: List<List<Int>>, pos: Position): Boolean {
    for (col in 0..pos.col - 1) {
        if (forest[pos.row][pos.col] <= forest[pos.row][col]) return false
    }
    return true
}

fun isVisibleFromLeft(forest: List<List<Int>>, pos: Position): Boolean {
    for (col in forest[0].size - 1 downTo pos.col + 1) {
        if (forest[pos.row][pos.col] <= forest[pos.row][col]) return false
    }
    return true
}

fun isVisibleFromTop(forest: List<List<Int>>, pos: Position): Boolean {
    for (row in 0..pos.row - 1) {
        if (forest[pos.row][pos.col] <= forest[row][pos.col]) return false
    }
    return true
}

fun isVisibleFromBottom(forest: List<List<Int>>, pos: Position): Boolean {
    for (row in forest.size - 1 downTo pos.row + 1) {
        if (forest[pos.row][pos.col] <= forest[row][pos.col]) return false
    }
    return true
}
