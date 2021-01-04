// run with: scala {-cp classes} Problem1 `cat 2.txt`

object Problem2 extends App {
    println(args.map(p => calculateRibbon(p.split("x").map(_.toInt))).sum);

    def calculateRibbon(dimensions : Array[Int]) : Int = {
        return calculateWrap(dimensions.sorted.take(2)) + calculateBow(dimensions);
    }

    def calculateWrap(dimensions: Array[Int]) : Int = {
        return 2 * (dimensions(0) + dimensions(1));
    }

    def calculateBow(dimensions : Array[Int]) : Int = {
        return dimensions.foldLeft(1)((a, v) => a * v);
    }
}