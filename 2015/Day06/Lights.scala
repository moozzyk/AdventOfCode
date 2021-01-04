object Lights {
    private val pattern = """^(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)$""".r;

    def turnOn(input: Iterator[String], lightOperation: (Int, String) => Int) : Array[Array[Int]] = {
        val lights = Array.ofDim[Int](1000, 1000);
        input.foreach(switch(_, lights, lightOperation));
        return lights;
    }

    private def switch(input: String, lights: Array[Array[Int]], lightOperation: (Int, String) => Int) {
        input match {
            case pattern(operation, topX, topY, bottomX, bottomY) => {
                for(x <- topX.toInt to bottomX.toInt) {
                    for (y <- topY.toInt to bottomY.toInt) {
                        lights(x)(y) = lightOperation(lights(x)(y), operation);
                    }
                }
            }
        }
    }
}