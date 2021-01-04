object CircuitBuilder {
    private val constPattern = """^(\d+) -> ([a-z]++)""".r;
    private val transitionPattern = """^([a-z]+) -> ([a-z]+)""".r;
    private val andPattern = """([a-z]+) AND ([a-z]+) -> ([a-z]+)""".r;
    private val and1Pattern = """1 AND ([a-z]+) -> ([a-z]+)""".r;
    private val orPattern = """([a-z]+) OR ([a-z]+) -> ([a-z]+)""".r;
    private val lshiftPattern = """([a-z]+) LSHIFT (\d+) -> ([a-z]+)""".r;
    private val rshiftPattern = """([a-z]+) RSHIFT (\d+) -> ([a-z]+)""".r;
    private val notPattern = """NOT ([a-z]+) -> ([a-z]+)""".r;

    def build(input: Iterator[String]): scala.collection.mutable.Map[String, () =>  Int] = {
        return build(input, scala.collection.mutable.Map[String, Int]());
    }

    def build(input: Iterator[String], cache: scala.collection.mutable.Map[String, Int]): scala.collection.mutable.Map[String, () =>  Int] = {
        val circuit = scala.collection.mutable.Map[String, () => Int]();
        input.foreach(buildGate(_, circuit, cache));
        return circuit;
    }

    private def buildGate(
        definition: String,
        circuit: scala.collection.mutable.Map[String, () => Int],
        cache: scala.collection.mutable.Map[String, Int]) = {
        definition match {
            case constPattern(value, output) => circuit(output) = buildWithCache(output, cache, buildConst(value, output))
            case transitionPattern(input, output) => circuit(output) = buildWithCache(output, cache, buildTransition(input, circuit, output))
            case and1Pattern(input, output) => circuit(output) = buildWithCache(output, cache, buildAnd1(input, circuit, output))
            case andPattern(input1, input2, output) => circuit(output) = buildWithCache(output, cache, buildAnd(input1, input2, circuit, output))
            case orPattern(input1, input2, output) => circuit(output) = buildWithCache(output, cache, buildOr(input1, input2, circuit, output))
            case notPattern(input, output) => circuit(output) = buildWithCache(output, cache, buildNot(input, circuit, output))
            case lshiftPattern(input, bits, output) => circuit(output) = buildWithCache(output, cache, buildLeftShift(input, bits.toInt, circuit, output))
            case rshiftPattern(input, bits, output) => circuit(output) = buildWithCache(output, cache, buildRightShift(input, bits.toInt, circuit, output))
            case _ => throw new IllegalStateException(s"Pattern \'${definition}\' not matched.")
        }
    }

    private def buildWithCache(key: String, cache: scala.collection.mutable.Map[String, Int], operation: () => Int): () => Int = {
        () => {
            if (!cache.keySet.exists(_ == key)) {
                cache(key) = operation();
            }
            cache(key);
        }
    }

    private def buildConst(value: String, output: String): () => Int = {
        () => value.toInt;
    }

    private def buildTransition(
        input: String,
        circuit: scala.collection.mutable.Map[String, () => Int],
        output: String): () => Int = {
            () => circuit(input)();
    }

    private def buildAnd1(
        input: String,
        circuit: scala.collection.mutable.Map[String, () => Int],
        output: String): () => Int = {
            () => circuit(input)() & 1;
    }

    private def buildAnd(
        input1: String,
        input2: String,
        circuit: scala.collection.mutable.Map[String, () => Int],
        output: String): () => Int = {
            () => circuit(input1)() & circuit(input2)();
    }

    private def buildOr(
        input1: String,
        input2: String,
        circuit: scala.collection.mutable.Map[String, () => Int],
        output: String): () => Int = {
            () => circuit(input1)() | circuit(input2)();
    }

    private def buildNot(
        input: String,
        circuit: scala.collection.mutable.Map[String, () => Int],
        output: String): () => Int = {
            () => ~circuit(input)() & 0xffff;
    }

    private def buildLeftShift(
        input: String,
        bits: Int,
        circuit: scala.collection.mutable.Map[String, () => Int],
        output: String): () => Int = {
            () => circuit(input)() << bits;
    }

    private def buildRightShift(
        input: String,
        bits: Int,
        circuit: scala.collection.mutable.Map[String, () => Int],
        output: String): () => Int = {
            () => circuit(input)() >> bits;
    }
}