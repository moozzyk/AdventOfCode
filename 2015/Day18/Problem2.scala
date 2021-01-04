object Problem2 extends App {

    val inputLines = scala.io.Source.fromFile(args(0)).getLines.toList;
    var lights = inputLines.map(l => l.toArray).toArray

    for (i <- 0 until args(1).toInt) {
        lights = LightAnimator.animate(lights);
        lights(0)(0) = '#';
        lights(0)(lights.size - 1) = '#';
        lights(lights.size - 1)(0) = '#';
        lights(lights.size - 1)(lights.size - 1) = '#';
    }

    println(lights.flatten.foldLeft(0)((t, v) => t + (if (v == '#') 1 else 0)))
}

