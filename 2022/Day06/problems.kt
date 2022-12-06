import java.io.File

fun main(args: Array<String>) {
    val input = File(args[0]).readText()
    println(problem1(input))
    println(problem2(input))
}

fun problem1(dataStream: String): Int = problem(dataStream, 4)

fun problem2(dataStream: String): Int = problem(dataStream, 14)

fun problem(dataStream: String, markerLength: Int): Int =
        markerLength +
                dataStream.windowed(markerLength).map { it.toSet().size }.indexOf(markerLength)
