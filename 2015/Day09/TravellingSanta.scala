object TravellingSanta {
    private val distancePattern = """^(\S+) to (\S+) = (\d+)$""".r;

    def getDistances(cityPairs: Iterator[String]): List[Int] = {
        val distances = createDistanceMap(cityPairs);
        RouteFinder.findRoutes(distances.keySet.map(t => t._1)).map(r => calculateDistance(r, distances))
    }

    private def createDistanceMap(distances: Iterator[String]) = {
        distances.map(parseInput(_)).flatten.toMap
    }

    private def parseInput(input: String) = {
        input match {
            case distancePattern(origin, destination, distance) =>
                List(((origin, destination), distance.toInt), ((destination, origin), distance.toInt))
        }
    }

    private def calculateDistance(route: List[String], distances: Map[(String, String), Int]): Int = {
        route.sliding(2).map(s => distances((s(0), s(1)))).sum
    }
}