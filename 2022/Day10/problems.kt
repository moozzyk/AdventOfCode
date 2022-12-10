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
