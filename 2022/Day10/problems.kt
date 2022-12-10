import java.io.File

class CPU(val program: List<String>) {
    var x: Int = 1
    var totalCycles: Int = 1
    var ip: Int = 0
    var cycleRemaining: Int = 0

    fun cycle(): Boolean {
        if (ip == program.size) {
            return false
        }

        if (cycleRemaining == 0) {
            cycleRemaining = if (program[ip] == "noop") 1 else 2
        }
        cycleRemaining--
        totalCycles++
        if (cycleRemaining == 0) {
            if (program[ip] != "noop") {
                x += program[ip].split(" ")[1].toInt()
            }
            ip++
        }
        return true
    }
}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    println(problem1(lines))
    problem2(lines)
}

fun problem1(program: List<String>): Int {
    val cpu = CPU(program)
    var totalSignalStrength = 0
    while (cpu.cycle()) {
        if (((cpu.totalCycles - 20) % 40) == 0) {
            val signalStrength = cpu.totalCycles * cpu.x
            totalSignalStrength += signalStrength
        }
    }

    return totalSignalStrength
}

fun problem2(program: List<String>) {
    val cpu = CPU(program)
    var screen = mutableListOf<Char>()
    while (cpu.cycle()) {
        val pixel = (cpu.totalCycles - 1) % 40
        screen.add(if (cpu.x >= pixel - 1 && cpu.x <= pixel + 1) '#' else ' ')
    }
    screen.windowed(40, 40).map { it.joinToString("") }.forEach { println(it) }
}
