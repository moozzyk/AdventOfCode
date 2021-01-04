object Problem1 extends App {
    val happiness = (scala.io.Source.fromFile(args(0)).getLines).map(l => Helper.getHappiness(l)).toMap
    val people = happiness.keySet.map(k => k._1)

    println(Helper.findTableConfigurations(people.toList, Nil).map(c => Helper.calculateTableHappiness(c, happiness)).max)
}