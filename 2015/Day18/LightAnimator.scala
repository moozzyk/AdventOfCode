object LightAnimator {

    def animate(lights: Array[Array[Char]]) : Array[Array[Char]] = {
        val newArray = Array.ofDim[Char](lights.size, lights.size)

        for (i <- 0 until lights.size; j <- 0 until lights.size) {
            newArray(i)(j) = if (getNewState(lights, i, j)) '#' else '.'
        }

        newArray
    }

    private def getNewState(lights: Array[Array[Char]], row: Int, col: Int) = {
        var lightsOn = 0;
        for (i <- Math.max(0, row - 1) until Math.min(lights.size, row + 2)) {
            for(j <- Math.max(0, col - 1) until Math.min(lights.size, col + 2)) {
                if (i != row || j != col) {
                    if (lights(i)(j) == '#') {
                        lightsOn = lightsOn + 1
                    }
                }
            }
        }

       lightsOn == 3 || (lights(row)(col) == '#' && lightsOn == 2)
    }
}