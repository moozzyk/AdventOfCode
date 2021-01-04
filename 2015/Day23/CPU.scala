class CPU {

    var RegisterA: Int = 0
    var RegisterB: Int = 0
    var InstructionPointer: Int = 0

    def run(program: Array[Instruction], a: Int) = {
        RegisterA = a
        RegisterB = 0
        InstructionPointer = 0

        while(InstructionPointer >= 0 && InstructionPointer < program.size) {
            execute(program(InstructionPointer))
        }
    }

    private def execute(instruction: Instruction) = {
        instruction.Code match {
            case "hlf" => half(instruction)
            case "tpl" => triple(instruction)
            case "inc" => increment(instruction)
            case "jmp" => jump(instruction)
            case "jie" => jumpIfEven(instruction)
            case "jio" => jumpIfOne(instruction)
        }
    }

    private def half(instruction: Instruction) = {
        registerOperation(instruction.Register, (r) => r >> 1);
        InstructionPointer = InstructionPointer + 1
    }

    private def triple(instruction: Instruction) = {
        registerOperation(instruction.Register, (r) => r * 3);
        InstructionPointer = InstructionPointer + 1
    }

    private def increment(instruction: Instruction) = {
        registerOperation(instruction.Register, (r) => r + 1);
        InstructionPointer = InstructionPointer + 1
    }

    private def jump(instruction: Instruction) = {
        InstructionPointer = InstructionPointer + instruction.Offset
    }

    private def jumpIfEven(instruction: Instruction) = {
        InstructionPointer = InstructionPointer +
            (if ((getRegisterValue(instruction.Register) & 1) == 0)
                instruction.Offset
            else
                1)
    }

    private def jumpIfOne(instruction: Instruction) = {
        InstructionPointer = InstructionPointer +
            (if (getRegisterValue(instruction.Register) == 1)
                instruction.Offset
            else
                1)
    }

    private def registerOperation(register: String, operation: (Int) => Int) {
        if (register == "a")
            RegisterA = operation(RegisterA)
        else
            RegisterB = operation(RegisterB)
    }

    private def getRegisterValue(register: String): Int = {
        if (register == "a")
            RegisterA
        else
            RegisterB
    }
}
