import java.io.File

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    problem1(lines)
    problem2(lines)
}

fun problem1(lines: List<String>) {
    println(calculateScore(lines))
}

fun problem2(lines: List<String>) {
    println(
            calculateScore(
                    lines.map {
                        // Input:
                        // A - Rock; B - Paper; C - Scissors
                        // X - Lose; Y - Draw; Z - Win
                        // Output:
                        // A, X - Rock; B, Y - Paper; C, Z: Scissors
                        when (it) {
                            "A X" -> "A Z"
                            "A Y" -> "A X"
                            "A Z" -> "A Y"
                            "B X" -> "B X"
                            "B Y" -> "B Y"
                            "B Z" -> "B Z"
                            "C X" -> "C Y"
                            "C Y" -> "C Z"
                            "C Z" -> "C X"
                            else -> ""
                        }
                    }
            )
    )
}

fun calculateScore(lines: List<String>): Int {
    val results =
            lines.map {
                when (it) {
                    // A, X - Rock; B, Y - Paper; C, Z: Scissors
                    "A X" -> 4
                    "A Y" -> 8
                    "A Z" -> 3
                    "B X" -> 1
                    "B Y" -> 5
                    "B Z" -> 9
                    "C X" -> 7
                    "C Y" -> 2
                    "C Z" -> 6
                    else -> 0
                }
            }
    return results.sum()
}
