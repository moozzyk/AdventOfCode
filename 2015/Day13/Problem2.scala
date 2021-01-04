object Problem2 extends App {
    var happiness = (scala.io.Source.fromFile(args(0)).getLines).map(l => Helper.getHappiness(l)).toMap
    happiness = happiness ++ happiness.keySet.map(k => ((k._1, "self"), 0)) ++ happiness.keySet.map(k => (("self", k._1), 0))

    val people = happiness.keySet.map(k => k._1) + ("self")

    println(Helper.findTableConfigurations(people.toList, Nil).map(c => Helper.calculateTableHappiness(c, happiness)).max)
}