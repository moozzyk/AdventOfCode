object Problem1 extends App {
    println(TravellingSanta.getDistances(scala.io.Source.fromFile(args(0)).getLines).min)
}