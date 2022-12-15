import java.io.File
import kotlin.math.*

data class Position(val x: Int, val y: Int) {}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val regex = """x=(-?\d+).*y=(-?\d+).*x=(-?\d+).*y=(-?\d+)""".toRegex()
    val input =
            lines.map {
                regex.find(it)!!.destructured.let {
                        (sensorX: String, sensorY: String, beaconX: String, beaconY: String) ->
                    Pair(
                            Position(sensorX.toInt(), sensorY.toInt()),
                            Position(beaconX.toInt(), beaconY.toInt())
                    )
                }
            }
    println(problem1(input, args[1].toInt()))
}

fun problem1(input: List<Pair<Position, Position>>, targetRow: Int): Int {
    val intervals =
            input
                    .map { (sensor: Position, beacon: Position) ->
                        val dist = abs(sensor.x - beacon.x) + abs(sensor.y - beacon.y)
                        val spanY = Pair(sensor.y - dist, sensor.y + dist)
                        if (targetRow < spanY.first || targetRow > spanY.second) null
                        else {
                            val half = dist - abs(targetRow - sensor.y)
                            Pair<Int, Int>(sensor.x - half, sensor.x + half)
                        }
                    }
                    .filter { it != null }
                    .map { it!! }
                    .sortedBy { it.first }

    var mergedIntervals = mutableListOf(intervals.first())
    intervals.forEach { (from: Int, to: Int) ->
        val (lastFrom, lastTo) = mergedIntervals.last()
        if (from > lastTo) {
            mergedIntervals.add(Pair<Int, Int>(from, to))
        } else {
            mergedIntervals.removeAt(mergedIntervals.size - 1)
            mergedIntervals.add(Pair<Int, Int>(lastFrom, max(lastTo, to)))
        }
    }

    return mergedIntervals.map { (from: Int, to: Int) -> to - from }.sum()
}
