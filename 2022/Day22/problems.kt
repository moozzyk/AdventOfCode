import java.io.File

enum class Direction {
    RIGHT,
    DOWN,
    LEFT,
    UP
}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    var map = createMap(lines)
    var path = getPath(lines)
    println(problem1(map, path))
    println(problem2(map, path))
}

fun createMap(lines: List<String>): List<CharArray> {
    var map = lines.takeWhile { it != "" }
    var maxWidth = map.map { it.length }.max()
    return map.map { it + " ".repeat(maxWidth - it.length) }.map { it.toCharArray() }.toList()
}

fun getPath(lines: List<String>): String {
    return lines.dropWhile { it != "" }[1]
}

fun problem1(map: List<CharArray>, path: String): Int {
    var row = 0
    var col = map[0].indexOf('.')
    var direction = Direction.RIGHT
    var pathIdx = 0

    while (pathIdx < path.length) {
        var steps = 0
        do {
            steps *= 10
            steps += path[pathIdx].toString().toInt()
            pathIdx++
        } while (pathIdx < path.length && path[pathIdx].isDigit())

        for (i in 0 until steps) {
            var nextRow = row
            var nextCol = col
            if (direction == Direction.UP) {
                nextRow--
                if (nextRow < 0 || map[nextRow][nextCol] == ' ') {
                    nextRow = map.size - 1
                    while (map[nextRow][nextCol] == ' ') {
                        nextRow--
                    }
                }
            } else if (direction == Direction.DOWN) {
                nextRow++
                if (nextRow == map.size || map[nextRow][nextCol] == ' ') {
                    nextRow = 0
                    while (map[nextRow][nextCol] == ' ') {
                        nextRow++
                    }
                }
            } else if (direction == Direction.LEFT) {
                nextCol--
                if (nextCol < 0 || map[nextRow][nextCol] == ' ') {
                    nextCol = map[nextRow].size - 1
                    while (map[nextRow][nextCol] == ' ') {
                        nextCol--
                    }
                }
            } else if (direction == Direction.RIGHT) {
                nextCol++
                if (nextCol == map[nextRow].size || map[nextRow][nextCol] == ' ') {
                    nextCol = 0
                    while (map[nextRow][nextCol] == ' ') {
                        nextCol++
                    }
                }
            }

            if (map[nextRow][nextCol] == '#') {
                break
            }
            row = nextRow
            col = nextCol
        }

        if (pathIdx < path.length) {
            direction =
                    if (path[pathIdx] == 'L') {
                        Direction.values()[(4 + direction.ordinal - 1) % 4]
                    } else {
                        Direction.values()[(direction.ordinal + 1) % 4]
                    }
            pathIdx++
        }
    }

    return (row + 1) * 1000 + (col + 1) * 4 + direction.ordinal
}

fun problem2(map: List<CharArray>, path: String): Int {
    var dir = 0
    var pathIdx = 0
    var row = 0
    var col = map[0].indexOf('.')

    while (pathIdx < path.length) {
        var steps = 0
        do {
            steps *= 10
            steps += path[pathIdx].toString().toInt()
            pathIdx++
        } while (pathIdx < path.length && path[pathIdx].isDigit())

        for (i in 0 until steps) {
            val (nextRow, nextCol, nextDir) = nextMove(row, col, dir)
            if (map[nextRow][nextCol] == '.') {
                row = nextRow
                col = nextCol
                dir = nextDir
            } else if (map[nextRow][nextCol] != '#') {
                throw Exception("Invalid position")
            }
        }

        if (pathIdx < path.length) {
            dir = (4 + dir + (if (path[pathIdx] == 'L') -1 else 1)) % 4
            pathIdx++
        }
    }

    return (row + 1) * 1000 + (col + 1) * 4 + dir
}

fun nextMove(row: Int, col: Int, dir: Int): Triple<Int, Int, Int> {
    val sideSize = 50
    val dRow = listOf(0, 1, 0, -1)
    val dCol = listOf(1, 0, -1, 0)
    val nextRow = row + dRow[dir]
    val nextCol = col + dCol[dir]
    val currSector = Pair(row / sideSize, col / sideSize)
    val nextSector = Pair(nextRow.floorDiv(sideSize), nextCol.floorDiv(sideSize))
    val sideRow = row % sideSize
    val sideCol = col % sideSize

    if (currSector == Pair(0, 1)) {
        if (nextSector == Pair(-1, 1)) {
            return Triple(3 * sideSize + sideCol, 0, Direction.RIGHT.ordinal)
        }
        if (nextSector == Pair(0, 0)) {
            return Triple(3 * sideSize - 1 - sideRow, 0, Direction.RIGHT.ordinal)
        }
    }
    if (currSector == Pair(0, 2)) {
        if (nextSector == Pair(-1, 2)) {
            return Triple(4 * sideSize - 1, sideCol, Direction.UP.ordinal)
        }
        if (nextSector == Pair(0, 3)) {
            return Triple((3 * sideSize - 1) - sideRow, 2 * sideSize - 1, Direction.LEFT.ordinal)
        }
        if (nextSector == Pair(1, 2)) {
            return Triple(sideSize + sideCol, 2 * sideSize - 1, Direction.LEFT.ordinal)
        }
    }
    if (currSector == Pair(1, 1)) {
        if (nextSector == Pair(1, 0)) {
            return Triple(2 * sideSize, sideRow, Direction.DOWN.ordinal)
        }
        if (nextSector == Pair(1, 2)) {
            return Triple(sideSize - 1, (2 * sideSize) + sideRow, Direction.UP.ordinal)
        }
    }
    if (currSector == Pair(2, 0)) {
        if (nextSector == Pair(1, 0)) {
            return Triple(sideSize + sideCol, sideSize, Direction.RIGHT.ordinal)
        }
        if (nextSector == Pair(2, -1)) {
            return Triple(sideSize - 1 - sideRow, sideSize, Direction.RIGHT.ordinal)
        }
    }
    if (currSector == Pair(2, 1)) {
        if (nextSector == Pair(2, 2)) {
            return Triple(sideSize - 1 - sideRow, 3 * sideSize - 1, Direction.LEFT.ordinal)
        }
        if (nextSector == Pair(3, 1)) {
            return Triple(3 * sideSize + sideCol, sideSize - 1, Direction.LEFT.ordinal)
        }
    }
    if (currSector == Pair(3, 0)) {
        if (nextSector == Pair(3, -1)) {
            return Triple(0, sideSize + sideRow, Direction.DOWN.ordinal)
        }
        if (nextSector == Pair(3, 1)) {
            return Triple(3 * sideSize - 1, sideSize + sideRow, Direction.UP.ordinal)
        }
        if (nextSector == Pair(4, 0)) {
            return Triple(0, 2 * sideSize + sideCol, Direction.DOWN.ordinal)
        }
    }

    return Triple(nextRow, nextCol, dir)
}
