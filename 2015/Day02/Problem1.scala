// run with: scala {-cp classes} Problem1 `cat 2.txt`

object Problem1 extends App {
    println(args.map(p => calculateWrapping(p.split("x").map(_.toInt))).sum);

    def calculateWrapping(dimensions : Array[Int]) : Int = {
        return 2 * dimensions(0) * dimensions(1) + 2 * dimensions(0) * dimensions(2) + 2 * dimensions(1) * dimensions(2) + calculateSlack(dimensions.sorted.take(2));
    }

    def calculateSlack(dimensions : Array[Int]) : Int = {
        return dimensions(0) * dimensions(1);
    }
}