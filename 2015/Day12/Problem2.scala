object Problem2 extends App {

    scala.util.parsing.json.JSON.parseFull(scala.io.Source.fromFile(args(0)).getLines.mkString) match {
        case Some(a: Any) => println(processJson(a))
        case _ => println("Error parsing JSon")
    }

    private def processJson(json: Any): Double = {
        json match {
            case l: List[Any] => l.foldLeft(0.0)((t, v) => t + processJson(v))
            case m: Map[String, Any] => if (m.values.exists(x => x == "red")) 0.0 else m.foldLeft(0.0)((t, v) => t + processJson(v._2))
            case n: Double => n
            case _ => 0
        }
    }
}