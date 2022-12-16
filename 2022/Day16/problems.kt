import java.io.File
import kotlin.math.*

data class Valve(val label: String, val flow: Int, val tunnels: Set<String>) {}

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
}

fun problem1(valveMap: Map<String, Valve>): Int {
    return openValves("AA", 1, valveMap, mutableMapOf<String, Int>())
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

fun openValves(
        currentValve: String,
        minute: Int,
        valveMap: Map<String, Valve>,
        openedValves: MutableMap<String, Int>
): Int {
    var maxFlow = calculateCurrentFlow(valveMap.values, openedValves)
    if (minute >= 30) {
        return maxFlow
    }
    if (!openedValves.containsKey(currentValve) && valveMap[currentValve]!!.flow > 0) {
        openedValves[currentValve] = minute
        val flow = openValves(currentValve, minute + 1, valveMap, openedValves)
        openedValves.remove(currentValve)
        return flow
    }

    for (nextValve in
            valveMap.values.filter { it.flow > 0 && !openedValves.containsKey(it.label) }) {
        val cost = pathCost(currentValve, nextValve.label, valveMap)
        maxFlow = max(maxFlow, openValves(nextValve.label, minute + cost, valveMap, openedValves))
    }
    return maxFlow
}

fun calculateCurrentFlow(valves: Collection<Valve>, openedValves: Map<String, Int>): Int {
    var totalFlow = 0
    for (v in valves) {
        val step = openedValves.getOrDefault(v.label, 30)
        totalFlow += (30 - step) * v.flow
    }

    return totalFlow
}
