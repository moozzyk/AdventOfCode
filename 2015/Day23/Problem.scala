
object Problem extends App {

    val prg = loadProgram(scala.io.Source.fromFile(args(0)).getLines)
    val cpu = new CPU;
    cpu.run(prg, args(1).toInt);
    println(s"A: ${cpu.RegisterA}, B: ${cpu.RegisterB}")

    private def loadProgram(lines: Iterator[String]): Array[Instruction] = {
        val registerOnlyInstructionPattern = """(\S+) (a|b)""".r
        val jumpInstructionPattern = """(jmp) (\+|-)(\d+)""".r
        val conditionalJumpPattern = """(\S+) (a|b), (\+|-)(\d+)""".r

        lines.map(line => line match {
            case registerOnlyInstructionPattern(i, r) => new Instruction(i, r, Int.MinValue)
            case jumpInstructionPattern(i, s, o) => new Instruction(i, "", if (s == "+") o.toInt else -o.toInt)
            case conditionalJumpPattern(i, r, s, o) =>  new Instruction(i, r, if (s == "+") o.toInt else -o.toInt)
        }).toArray
    }
}