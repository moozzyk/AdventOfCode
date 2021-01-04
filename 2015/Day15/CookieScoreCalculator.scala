import scala.collection.mutable.ListBuffer

object CookieScoreCalculator {
    def scoreCookies(teaspoons: Int, ingredientCount: Int, ingredients: scala.collection.mutable.Map[String, ListBuffer[Int]]): List[(Int, Int)] = {
        scoreCookies(teaspoons, Nil, ingredientCount, ingredients)
    }

    private def scoreCookies(teaspoons: Int, partitions: List[Int], remainingPartitions: Int, ingredients: scala.collection.mutable.Map[String, ListBuffer[Int]]): List[(Int, Int)] = {
        if (remainingPartitions == 1) {
            List(calculateScore(partitions :+ teaspoons, ingredients))
        }
        else {
            (0 to teaspoons).map(i => scoreCookies(teaspoons - i, partitions :+ i, remainingPartitions - 1, ingredients)).toList.flatten
        }
    }

    private def calculateScore(teaspoons: List[Int], ingredients: scala.collection.mutable.Map[String, ListBuffer[Int]]): (Int, Int) = {
        val score = ingredients.keySet.filter(k => k != "calories").toList
            .map(k => ingredients(k).zip(teaspoons).map(p => p._1 * p._2).sum).map(x => Math.max(0, x))
            .foldLeft(1)((t, v) => t * v)

        val calories = ingredients("calories").zip(teaspoons).map(p => p._1 * p._2).sum;

        (score, calories)
    }
}