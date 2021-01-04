object Problem2 extends App {

    val inputLines = scala.io.Source.fromFile(args(0)).getLines.toList;
    val replacements = buildReverseReplacements(inputLines.take(inputLines.size - 2))
    val targetString = inputLines.last

    println(replacements)
    println(reduceString(targetString))

    private def reduceString(input: String): Int = {

        var steps = 0;
        var s = input;
        while (s.exists(_ != 'e')) {

                    println(s)


            val r = replacements.filter(r => s contains r._1).take(1)(0);

            var index = 0;
            while (index >= 0) {
                index = s.indexOf(r._1, index)
                if (index >= 0) {
                    s = s.substring(0, index) + r._2 + s.substring(index + r._1.length)
                    steps = steps + 1
                    index = index + r._2.length
                    println(s"${r._1} -> ${r._2} ${s}")
                }
            }

/*
            var found = false;
            for (r <- replacements) {
                val i = s indexOf r._1;
                if (!found && i >= 0) {
                    s = s.substring(0, i) + r._2 + s.substring(i + r._1.length)
                    println(s"${r._1} -> ${r._2} ${s}")
                    steps = steps + 1;
                }
            }
            */
        }

        return steps;
    }

    private def buildReverseReplacements(inputLines: List[String]): List[(String, String)] = {
        val linePattern = """(\S+) => (\S+)""".r
        inputLines
            .map { case linePattern(k, v) => (v, k) }
            .sortBy(- _._1.size)
    }
}