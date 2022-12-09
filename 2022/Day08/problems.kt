import java.io.File
import kotlin.math.max

data class Position(val row: Int, val col: Int) {}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val forest = lines.map { it.toCharArray().map { it.code - '0'.code } }
    println(problem1(forest))
    println(problem2(forest))

    // println(computeScenicScore(forest, 1, 2))
    // println(computeScenicScore(forest, 3, 2))
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

fun problem2(forest: List<List<Int>>): Int {
    var maxScore = 0
    for (row in 0..forest[0].size - 1) {
        for (col in 0..forest[0].size - 1) {
            val scenicScore = computeScenicScore(forest, row, col)
            maxScore = max(maxScore, scenicScore)
        }
    }
    return maxScore
}

fun computeScenicScore(forest: List<List<Int>>, row: Int, col: Int): Int {
    val pos = Position(row, col)
    // println(
    //         "(up)${numVisibleUp(forest, pos)} " +
    //                 "(left)${numVisibleLeft(forest, pos)} " +
    //                 "(down)${numVisibleDown(forest, pos)} " +
    //                 "(right)${numVisibleRight(forest, pos)} "
    // )

    return numVisibleUp(forest, pos) *
            numVisibleLeft(forest, pos) *
            numVisibleDown(forest, pos) *
            numVisibleRight(forest, pos)
}

fun numVisibleLeft(forest: List<List<Int>>, pos: Position): Int {
    for (col in pos.col - 1 downTo 0) {
        if (forest[pos.row][pos.col] <= forest[pos.row][col]) return pos.col - col
    }
    return pos.col
}

fun numVisibleRight(forest: List<List<Int>>, pos: Position): Int {
    for (col in pos.col + 1 until forest[0].size) {
        if (forest[pos.row][pos.col] <= forest[pos.row][col]) return col - pos.col
    }
    return forest[0].size - pos.col - 1
}

fun numVisibleUp(forest: List<List<Int>>, pos: Position): Int {
    for (row in pos.row - 1 downTo 0) {
        if (forest[pos.row][pos.col] <= forest[row][pos.col]) return pos.row - row
    }
    return pos.row
}

fun numVisibleDown(forest: List<List<Int>>, pos: Position): Int {
    for (row in pos.row + 1 until forest.size) {
        if (forest[pos.row][pos.col] <= forest[row][pos.col]) return row - pos.row
    }
    return forest.size - pos.row - 1
}
