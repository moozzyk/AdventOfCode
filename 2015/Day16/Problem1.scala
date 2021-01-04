object Problem1 extends App {
    val linePattern = """(Sue \d+): (\S+): (\d+), (\S+): (\d+), (\S+): (\d+)""".r

    val prototype = scala.collection.mutable.Map[String, Int]()
    prototype("children") = 3
    prototype("cats") =  7
    prototype("samoyeds") =  2
    prototype("pomeranians") =  3
    prototype("akitas") =  0
    prototype("vizslas") =  0
    prototype("goldfish") =  5
    prototype("trees") =  3
    prototype("cars") =  2
    prototype("perfumes") =  1

    (scala.io.Source.fromFile(args(0)).getLines).map(l => parseInput(l, prototype))
        .filter(m => m._1 == prototype).foreach(x => println(x._2))

    private def parseInput(line: String, prototype: scala.collection.mutable.Map[String, Int]) = {
        var newItem = prototype.clone;
        line match {
            case linePattern(name, p1, v1, p2, v2, p3, v3) => {
                newItem(p1) = v1.toInt
                newItem(p2) = v2.toInt
                newItem(p3) = v3.toInt
                (newItem, name)
            }
        }
    }
}