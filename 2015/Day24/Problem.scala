object Problem extends App {

    val numbers = scala.io.Source.fromFile(args(0)).getLines.map(_.toLong).toList.sorted.toList
    val partitions = args(1).toInt
    val partitionSum = numbers.sum / partitions

    val firstSet = findCombinations(numbers, partitionSum, partitions)
        .groupBy(_.size)
        .toList.sortBy(_._1).head._2    // get combinations with fewest items

    println(firstSet.map(_.product).sorted.head)

    private def findCombinations(set: List[Long], sum: Long, partitions: Int): Seq[List[Long]] = {

        val minItems = set.sortWith(_ > _).scanLeft(0L)((t, v) => t + v).filter(_ < sum).size

        (minItems to set.size / partitions)
            .flatMap(set.combinations(_).filter(_.sum == sum))
    }
}