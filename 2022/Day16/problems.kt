import java.io.File
import kotlin.math.*

data class Valve(val label: String, val flow: Int, val tunnels: Set<String>)

class Solution(val valveMap: Map<String, Valve>) {
    val paths = cachePaths(valveMap)
    var closedValves = mutableMapOf<String, Valve>()
    var memo = mutableMapOf<Triple<String, Pair<String, Int>, Pair<String, Int>>, Int>()

    fun openValves(withHelp: Boolean, timeLimit: Int): Int {
        closedValves = valveMap.filter { it.value.flow > 0 }.toMutableMap()
        memo = mutableMapOf<Triple<String, Pair<String, Int>, Pair<String, Int>>, Int>()
        return openValves(
                valveMap["AA"]!!,
                1,
                valveMap["AA"]!!,
                if (withHelp) 1 else Int.MAX_VALUE,
                timeLimit
        )
    }

    fun openValves(
            valve1: Valve,
            minute1: Int,
            valve2: Valve,
            minute2: Int,
            timeLimit: Int,
    ): Int {
        if (minute1 > timeLimit) {
            return 0
        }
        var flow = (timeLimit - minute1 + 1) * valve1.flow

        var maxDescFlow =
                if (!closedValves.isEmpty()) {
                    0
                } else {
                    openValves(valve2, minute2, valve2, Int.MAX_VALUE, timeLimit)
                }
        for (nextValve in closedValves.values.toList()) {
            closedValves.remove(nextValve.label)
            val nextEta = 1 + minute1 + paths[Pair(valve1.label, nextValve.label)]!!
            val descFlow =
                    if (nextEta < minute2) {
                        openValves(nextValve, nextEta, valve2, minute2, timeLimit)
                    } else {
                        openValves(valve2, minute2, nextValve, nextEta, timeLimit)
                    }
            maxDescFlow = max(maxDescFlow, descFlow)
            closedValves.put(nextValve.label, nextValve)
        }
        return flow + maxDescFlow
    }

    fun cachePaths(valveMap: Map<String, Valve>): MutableMap<Pair<String, String>, Int> {
        var paths = mutableMapOf<Pair<String, String>, Int>()
        for (from in valveMap.filter { it.value.flow > 0 || it.key == "AA" }.map { it.key }) {
            for (to in valveMap.filter { it.value.flow > 0 }.map { it.key }) {
                if (from != to) {
                    paths.put(Pair(from, to), pathCost(from, to, valveMap))
                }
            }
        }
        return paths
    }

    fun pathCost(from: String, to: String, valveMap: Map<String, Valve>): Int {
        var visited = mutableSetOf<String>()
        var q = ArrayDeque<Pair<String, Int>>()
        q.addLast(Pair<String, Int>(from, 0))
        while (!q.isEmpty()) {
            val (valve, steps) = q.removeFirst()
            if (valve == to) {
                return steps
            }
            for (nextValve in valveMap[valve]!!.tunnels) {
                if (!visited.contains(nextValve)) {
                    visited.add(nextValve)
                    q.addLast(Pair(nextValve, steps + 1))
                }
            }
        }
        throw Exception("Cannot find path from ${from} to ${to}")
    }
}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val regex = """ ([A-Z]{2}) .*=(\d+).*(valve|valves) (.*)""".toRegex()
    val valveMap =
            lines
                    .map {
                        regex.find(it)!!.destructured.let {
                                (label: String, flow: String, _: String, tunnels: String) ->
                            Valve(label, flow.toInt(), tunnels.split(", ").toSet())
                        }
                    }
                    .associateBy({ it.label }, { it })

    println(problem1(valveMap))
    println(problem2(valveMap))
}

fun problem1(valveMap: Map<String, Valve>): Int {
    return Solution(valveMap).openValves(false, 30)
}

fun problem2(valveMap: Map<String, Valve>): Int {
    println("This is ridiculously slooow")
    return Solution(valveMap).openValves(true, 26)
}
