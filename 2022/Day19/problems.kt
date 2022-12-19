import java.io.File
import kotlin.math.*

data class Resources(val ore: Int, val clay: Int, val obsidian: Int) {
    operator fun plus(r: Resources): Resources {
        return Resources(ore + r.ore, clay + r.clay, obsidian + r.obsidian)
    }

    operator fun minus(r: Resources): Resources {
        return Resources(ore - r.ore, clay - r.clay, obsidian - r.obsidian)
    }

    operator fun times(m: Int): Resources {
        return Resources(ore * m, clay * m, obsidian * m)
    }
}

typealias Cost = Resources

data class Blueprint(
        val id: Int,
        val oreRobotCost: Cost,
        val clayRobotCost: Cost,
        val obsidianRobotCost: Cost,
        val geodeRobotCost: Cost
)

data class Robots(
        val oreRobots: Int,
        val clayRobots: Int,
        val obsidianRobots: Int,
        val geodeRobots: Int
) {
    operator fun plus(r: Robots): Robots {
        return Robots(
                oreRobots + r.oreRobots,
                clayRobots + r.clayRobots,
                obsidianRobots + r.obsidianRobots,
                geodeRobots + r.geodeRobots
        )
    }
    fun mine(): Resources {
        return Resources(oreRobots, clayRobots, obsidianRobots)
    }
}

class Factory(var blueprint: Blueprint) {}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val blueprints = createBlueprints(lines)
    println(problem1(blueprints))
}

fun createBlueprints(lines: List<String>): List<Blueprint> {
    val regex =
            """Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.""".toRegex()
    return lines.map { regex.matchEntire(it)!!.destructured }.map {
            (
                    id: String,
                    oreRobotOreCost: String,
                    clayRobotOreCost: String,
                    obsidianRobotOreCost: String,
                    obsidianRobotClayCost: String,
                    geodeRobotOreCost: String,
                    geodeRobotObsidianCost: String) ->
        Blueprint(
                id.toInt(),
                oreRobotCost = Cost(ore = oreRobotOreCost.toInt(), clay = 0, obsidian = 0),
                clayRobotCost = Cost(ore = clayRobotOreCost.toInt(), clay = 0, obsidian = 0),
                obsidianRobotCost =
                        Cost(
                                ore = obsidianRobotOreCost.toInt(),
                                clay = obsidianRobotClayCost.toInt(),
                                obsidian = 0
                        ),
                geodeRobotCost =
                        Cost(
                                ore = geodeRobotOreCost.toInt(),
                                clay = 0,
                                obsidian = geodeRobotObsidianCost.toInt()
                        )
        )
    }
}

fun problem1(blueprints: List<Blueprint>): Int {
    var results =
            blueprints.map {
                Pair(it, executeBlueprint(it, 1, Robots(1, 0, 0, 0), Resources(0, 0, 0)))
            }
    return results.map { (blueprint, result) -> blueprint.id * result }.sum()
}

fun executeBlueprint(blueprint: Blueprint, minute: Int, robots: Robots, resources: Resources): Int {
    var geodes = robots.geodeRobots
    if (minute == 24) {
        return geodes
    }

    val purchaseOptions = robotPurchaseOptions(blueprint, resources)
    var maxDescGeodes = 0
    for (p in purchaseOptions) {
        val cost = robotCost(p, blueprint)
        val descGeodes =
                executeBlueprint(
                        blueprint,
                        minute + 1,
                        robots + p,
                        resources + robots.mine() - cost
                )

        maxDescGeodes = max(descGeodes, maxDescGeodes)
    }

    return geodes + maxDescGeodes
}

fun robotPurchaseOptions(blueprint: Blueprint, resources: Resources): List<Robots> {
    if (canBuy(resources, blueprint.geodeRobotCost)) {
        return listOf(Robots(0, 0, 0, 1))
    }

    if (canBuy(resources, blueprint.obsidianRobotCost)) {
        return listOf(Robots(0, 0, 1, 0), Robots(0, 0, 0, 0))
    }

    if (canBuy(resources, blueprint.clayRobotCost) && canBuy(resources, blueprint.oreRobotCost)) {
        // Heurestics to avoid too many turns without buying any robot
        if (resources.ore > 6) {
            return listOf(Robots(1, 0, 0, 0), Robots(0, 1, 0, 0))
        }
        return listOf(Robots(1, 0, 0, 0), Robots(0, 1, 0, 0), Robots(0, 0, 0, 0))
    }

    if (canBuy(resources, blueprint.clayRobotCost)) {
        return listOf(Robots(0, 1, 0, 0), Robots(0, 0, 0, 0))
    }

    if (canBuy(resources, blueprint.oreRobotCost)) {
        return listOf(Robots(1, 0, 0, 0), Robots(0, 0, 0, 0))
    }

    return listOf(Robots(0, 0, 0, 0))
}

fun robotCost(robots: Robots, blueprint: Blueprint): Cost {
    return blueprint.oreRobotCost * robots.oreRobots +
            blueprint.clayRobotCost * robots.clayRobots +
            blueprint.obsidianRobotCost * robots.obsidianRobots +
            blueprint.geodeRobotCost * robots.geodeRobots
}

fun canBuy(robots: Robots, resources: Resources, blueprint: Blueprint): Boolean {
    return canBuy(resources, robotCost(robots, blueprint))
}

fun canBuy(resources: Resources, cost: Cost): Boolean {
    return cost.ore <= resources.ore &&
            cost.clay <= resources.clay &&
            cost.obsidian <= resources.obsidian
}
