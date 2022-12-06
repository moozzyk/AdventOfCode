import java.io.File

fun main(args: Array<String>) {
    val input = File(args[0]).readText()
    println(problem1(input))
}

fun problem1(dataStream: String): Int {
    return dataStream.windowed(4).map { it.toSet().size }.indexOf(4) + 4
}
