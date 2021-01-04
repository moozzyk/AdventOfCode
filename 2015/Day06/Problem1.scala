object Problem1 extends App {
    println(lightsOnCount(scala.io.Source.fromFile(args(0)).getLines))

    def lightsOnCount(input: Iterator[String]): Int = {
        return Lights.turnOn(input, lightFunction).foldLeft(0)((t, v) => t + v.sum);
    }

    def lightFunction(light: Int, operation: String): Int = operation match {
        case "turn on" => 1
        case "turn off" => 0
        case "toggle" => ~light & 0x1
    }
}