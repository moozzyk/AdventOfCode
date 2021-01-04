object Problem2 extends App {
    var inputLines = scala.io.Source.fromFile(args(0)).getLines.toList
    println(
        CookieScoreCalculator.scoreCookies(100, inputLines.size, InputReader.getIngredientMap(inputLines))
            .filter(p => p._2 == 500).max)
}