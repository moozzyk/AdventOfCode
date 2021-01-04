object Problem2 extends App {

    val containers = scala.io.Source.fromFile(args(1)).getLines.map(l => l.toInt).toList

    println(Helper.countCombinations(args(0).toInt, containers)
        .groupBy(i => i).mapValues(_.size).minBy(_._1)._2)
}