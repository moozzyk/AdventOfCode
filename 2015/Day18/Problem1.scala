object Problem1 extends App {

    val inputLines = scala.io.Source.fromFile(args(0)).getLines.toList;
    var lights = inputLines.map(l => l.toArray).toArray

    for (i <- 0 until args(1).toInt) {
        lights = LightAnimator.animate(lights);
    }

    println(lights.flatten.foldLeft(0)((t, v) => t + (if (v == '#') 1 else 0)))
}

