object RouteFinder extends App {
    def findRoutes(cities: Set[String]): List[List[String]] = findRoutes(cities.toList, Nil)

    private def findRoutes(cities: List[String], route: List[String]): List[List[String]] = {
        if (cities.isEmpty) {
            List(route);
        }
        else {
            cities.map(c => findRoutes(cities.filterNot(i => i == c), route :+ c)).flatten;
        }
    }
}