object Problem1 extends App {

    val inputLines = scala.io.Source.fromFile(args(0)).getLines.toList;
    val replacements = buildReplacements(inputLines.take(inputLines.size - 2))
    val input = inputLines.last

    println(findReplacements(input, replacements).flatten.flatten.distinct.size)

    private def buildReplacements(inputLines: List[String]) = {
        val linePattern = """(\S+) => (\S+)""".r
        inputLines
            .map { case linePattern(k, v) => (k, v) }
            .groupBy(_._1)
            .map { case (k, v) => (k, v.map(_._2))}
    }

    private def findReplacements(input: String, replacements: Map[String, List[String]]): IndexedSeq[Option[List[String]]] = {
        findReplacements(input, 1, replacements) ++ findReplacements(input, 2, replacements)
    }

    private def findReplacements(input: String, keySize: Int, replacements: Map[String, List[String]]) : IndexedSeq[Option[List[String]]] = {
        for { i <- 0 until input.length() + 1 - keySize } yield {
            replacements.get(input.substring(i, i + keySize)).
                map(r => r.map(s => input.substring(0, i) + s + input.substring(i + keySize)))
        }
    }
}