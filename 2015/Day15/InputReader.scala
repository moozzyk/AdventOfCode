import scala.collection.mutable.ListBuffer

object InputReader {

    private val linePattern = """(\S+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

    def getIngredientMap(input: List[String]): scala.collection.mutable.Map[String, ListBuffer[Int]] = {
        val ingredients = scala.collection.mutable.Map[String, ListBuffer[Int]]();
        ingredients("capacity") = ListBuffer[Int]()
        ingredients("durability") = ListBuffer[Int]()
        ingredients("flavor") = ListBuffer[Int]()
        ingredients("texture") = ListBuffer[Int]()
        ingredients("calories") = ListBuffer[Int]()

        input.foreach(i => readIngredients(i, ingredients))

        ingredients
    }

    private def readIngredients(line: String, ingredients: scala.collection.mutable.Map[String, ListBuffer[Int]]) = {
        line match {
            case linePattern(name, capacity, durability, flavor, texture, calories) => {
                    ingredients("capacity") += capacity.toInt;
                    ingredients("durability") += durability.toInt;
                    ingredients("flavor") += flavor.toInt;
                    ingredients("texture") += texture.toInt;
                    ingredients("calories") += calories.toInt;
                }
        }
    }
}