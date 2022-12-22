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
}

fun createMap(lines: List<String>): List<CharArray> {
    var map = lines.takeWhile { it != "" } // .map { it.toCharArray() }.toList()
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
            map[row][col] = ">v<^"[direction.ordinal]
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
