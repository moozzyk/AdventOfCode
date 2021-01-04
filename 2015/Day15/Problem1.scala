object Problem1 extends App {
    var inputLines = scala.io.Source.fromFile(args(0)).getLines.toList
    println(
        CookieScoreCalculator.scoreCookies(100, inputLines.size, InputReader.getIngredientMap(inputLines))
            .map(p => p._1).max)
}