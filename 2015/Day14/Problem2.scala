object Problem2 extends App {
    private val linePattern = """(\S+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.""".r;

    val reindeerSpecs = scala.io.Source.fromFile(args(0)).getLines.map(l => getReindeerSpecs(l)).toList

    println(Range(1, args(1).toInt)
        .map(t => getScore(t, reindeerSpecs)).flatten
        .groupBy(identity).mapValues(_.size).toSeq.sortBy(_._2).reverse)

    def getReindeerSpecs(input: String): (String, Int, Int, Int) = {
        input match {
            case linePattern(n, s, d, r) => (n, s.toInt, d.toInt, r.toInt)
        }
    }

    def getScore(time: Int, reindeers: List[(String, Int, Int, Int)]) = {
        val ordered = reindeers.map(r => (r._1, getDistance(time, r._2, r._3, r._4))).sortBy(s => s._2).reverse.toList;
        ordered.takeWhile(o => o._2 == ordered(0)._2).map(i => i._1)
    }

    def getDistance(time: Int, speed: Int, flyTime: Int, restTime: Int): Int = {
        val intervals = time/(flyTime + restTime);
        val remainingTime = time % (flyTime + restTime);

        return intervals * flyTime * speed + Math.min(remainingTime, flyTime) * speed;
    }
}
