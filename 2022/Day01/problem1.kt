import java.io.File
import kotlin.math.*

fun main() {
    val lines = File("input.txt").readLines()
    val caloriesSorted = getCaloriesSorted(lines)
    println(caloriesSorted[0])
    println(caloriesSorted.take(3).sum())
}

fun getCaloriesSorted(lines: List<String>): List<Int> {
    val calories = mutableListOf<Int>()
    var sum = 0
    for (line in lines) {
        if (line.isEmpty()) {
            calories.add(sum)
            sum = 0
        } else {
            sum += line.toInt()
        }
    }
    calories.sortDescending()
    return calories
}
