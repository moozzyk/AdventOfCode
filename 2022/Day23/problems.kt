import java.io.File
import kotlin.math.*

data class Position(val row: Int, val col: Int)

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(problem1(createElfPositions(lines)))
}

fun createElfPositions(lines: List<String>): MutableSet<Position> {
    var elfPositions = mutableSetOf<Position>()
    for (row in 0 until lines.size) {
        for (col in 0 until lines[row].length) {
            if (lines[row][col] == '#') {
                elfPositions.add(Position(row, col))
            }
        }
    }
    return elfPositions
}

fun problem1(elfPositions: MutableSet<Position>): Int {
    for (round in 0 until 10) {
        val tmp = mutableMapOf<Position, MutableList<Position>>()
        for (elfPosition in elfPositions) {
            val proposedMove = proposeNextMove(elfPosition, elfPositions, round)
            if (proposedMove != null) {
                if (tmp.containsKey(proposedMove)) {
                    tmp[proposedMove]!!.add(elfPosition)
                } else {
                    tmp[proposedMove] = mutableListOf(elfPosition)
                }
            }
        }

        for ((proposedMove, elves) in tmp) {
            if (elves.size == 1) {
                elfPositions.remove(elves[0])
                elfPositions.add(proposedMove)
            }
        }
    }

    var minRow = Int.MAX_VALUE
    var maxRow = Int.MIN_VALUE
    var minCol = Int.MAX_VALUE
    var maxCol = Int.MIN_VALUE
    for ((row, col) in elfPositions) {
        minRow = min(minRow, row)
        maxRow = max(maxRow, row)
        minCol = min(minCol, col)
        maxCol = max(maxCol, col)
    }
    return (1 + maxRow - minRow) * (1 + maxCol - minCol) - elfPositions.size
}

fun proposeNextMove(elfPosition: Position, elfPositions: Set<Position>, round: Int): Position? {
    if (!hasNeighbor(elfPosition, elfPositions)) {
        return null
    }

    val moves = listOf(::proposeNorth, ::proposeSouth, ::proposeWest, ::proposeEast)
    for (i in 0..3) {
        val nextPos = moves[(round + i) % moves.size].invoke(elfPosition, elfPositions)
        if (nextPos != null) {
            return nextPos
        }
    }

    return null
}

fun proposeNorth(elfPosition: Position, elfPositions: Set<Position>): Position? =
        if (!hasNeighborNorth(elfPosition, elfPositions)) {
            Position(elfPosition.row - 1, elfPosition.col)
        } else {
            null
        }

fun proposeSouth(elfPosition: Position, elfPositions: Set<Position>): Position? =
        if (!hasNeighborSouth(elfPosition, elfPositions)) {
            Position(elfPosition.row + 1, elfPosition.col)
        } else {
            null
        }

fun proposeWest(elfPosition: Position, elfPositions: Set<Position>): Position? =
        if (!hasNeighborWest(elfPosition, elfPositions)) {
            Position(elfPosition.row, elfPosition.col - 1)
        } else {
            null
        }

fun proposeEast(elfPosition: Position, elfPositions: Set<Position>): Position? =
        if (!hasNeighborEast(elfPosition, elfPositions)) {
            Position(elfPosition.row, elfPosition.col + 1)
        } else {
            null
        }

fun hasNeighbor(elfPosition: Position, elfPositions: Set<Position>): Boolean {
    return hasNeighborNorth(elfPosition, elfPositions) ||
            hasNeighborSouth(elfPosition, elfPositions) ||
            hasNeighborWest(elfPosition, elfPositions) ||
            hasNeighborEast(elfPosition, elfPositions)
}

fun hasNeighborNorth(elfPosition: Position, elfPositions: Set<Position>): Boolean {
    var (row, col) = elfPosition
    for (c in col - 1..col + 1) {
        if (elfPositions.contains(Position(row - 1, c))) {
            return true
        }
    }
    return false
}

fun hasNeighborSouth(elfPosition: Position, elfPositions: Set<Position>): Boolean {
    var (row, col) = elfPosition
    for (c in col - 1..col + 1) {
        if (elfPositions.contains(Position(row + 1, c))) {
            return true
        }
    }
    return false
}

fun hasNeighborWest(elfPosition: Position, elfPositions: Set<Position>): Boolean {
    var (row, col) = elfPosition
    for (r in row - 1..row + 1) {
        if (elfPositions.contains(Position(r, col - 1))) {
            return true
        }
    }
    return false
}

fun hasNeighborEast(elfPosition: Position, elfPositions: Set<Position>): Boolean {
    var (row, col) = elfPosition
    for (r in row - 1..row + 1) {
        if (elfPositions.contains(Position(r, col + 1))) {
            return true
        }
    }
    return false
}

fun printElves(elves: Set<Position>) {
    for (row in 0 until 20) {
        for (col in 0 until 20) {
            if (elves.contains(Position(row, col))) {
                print("#")
            } else {
                print(".")
            }
        }
        println()
    }
    println()
}
