object Problem2 extends App {
    val linePattern = """(Sue \d+): (\S+): (\d+), (\S+): (\d+), (\S+): (\d+)""".r

    val prototype = Map("children" -> 3, "cats" -> 7, "samoyeds" -> 2, "pomeranians" -> 3, "akitas" -> 0, "vizslas" -> 0,
                    "goldfish" -> 5, "trees" -> 3, "cars" -> 2, "perfumes" -> 1)

    (scala.io.Source.fromFile(args(0)).getLines).map(l => parseInput(l))
        .filter(m => compareAunts(prototype, m._1)).foreach(x => println(x._2))

    private def compareAunts(a1: Map[String, Int], a2: Map[String, Int]): Boolean = {
      a1("children") == a2("children") &&
      a1("cats") < a2("cats") &&
      a1("samoyeds") == a2("samoyeds") &&
      a1("pomeranians") > a2("pomeranians") &&
      a1("akitas") == a2("akitas") &&
      a1("vizslas") == a2("vizslas") &&
      a1("goldfish") > a2("goldfish") &&
      a1("trees") < a2("trees") &&
      a1("cars") == a2("cars") &&
      a1("perfumes") == a2("perfumes")
    }

    private def parseInput(line: String) = {
        line match {
            case linePattern(name, p1, v1, p2, v2, p3, v3) =>
                (Map("children" -> 3, "cats" -> Int.MaxValue, "samoyeds" -> 2, "pomeranians" -> Int.MinValue, "akitas" -> 0, "vizslas" -> 0,
                    "goldfish" -> Int.MinValue, "trees" -> Int.MaxValue, "cars" -> 2, "perfumes" -> 1, p1 -> v1.toInt, p2 -> v2.toInt, p3 -> v3.toInt), name)
            }
    }
}