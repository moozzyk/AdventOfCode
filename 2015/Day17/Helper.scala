object Helper {
    def countCombinations(volume: Int, containers: List[Int], containersUsed: Int = 0) : List[Int] = {

        if (volume == 0) {
            return List(containersUsed)
        }

        if (volume < 0 || containers.isEmpty) {
            return Nil
        }

        return (0 to containers.size - 1)
            .map(i => countCombinations(volume - containers(i), containers.drop(i + 1), containersUsed + 1)).toList.flatten
    }
}