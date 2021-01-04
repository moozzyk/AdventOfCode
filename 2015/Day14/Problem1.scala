object Problem1 extends App {
    private val linePattern = """(\S+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.""".r;

    println(scala.io.Source.fromFile(args(0)).getLines
        .map(l => getReindeerSpecs(l))
        .map(s => getDistance(args(1).toInt, s._2, s._3, s._4)).max)

    def getReindeerSpecs(input: String): (String, Int, Int, Int) = {
        input match {
            case linePattern(n, s, d, r) => (n, s.toInt, d.toInt, r.toInt)
        }
    }

    def getDistance(time: Int, speed: Int, flyTime: Int, restTime: Int): Int = {
        val intervals = time/(flyTime + restTime);
        val remainingTime = time % (flyTime + restTime);

        return intervals * flyTime * speed + Math.min(remainingTime, flyTime) * speed;
    }
}
