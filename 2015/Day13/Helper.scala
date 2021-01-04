object Helper {
    private val linePattern = """(\S+) would (\S+) (\d+) happiness units by sitting next to (\S+)\.""".r

    def getHappiness(input: String): ((String, String), Int) = {
        input match {
            case linePattern(p1, a, h, p2) => ((p1, p2), if (a == "gain") h.toInt else -(h.toInt))
        }
    }

    def findTableConfigurations(people: List[String], table: List[String]): List[List[String]] = {
        if (people.isEmpty) {
            List(table);
        }
        else {
            people.map(p => findTableConfigurations(people.filterNot(i => i == p), table :+ p)).flatten;
        }
    }

    def calculateTableHappiness(people: List[String], happiness: Map[(String, String), Int]) : Int = {
        happiness((people(0), people.last)) + happiness((people.last, people(0))) +
        people.sliding(2).map(s => happiness((s(0), s(1))) + happiness((s(1), s(0)))).sum
    }
}