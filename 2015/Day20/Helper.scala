object Helper {

    def getHouseNumber(isRightHouse: (Seq[Int], Int) => Boolean): Int = {
        var houseNumber = 1
        while (!isRightHouse(generateDivisors(houseNumber), houseNumber)) {
            houseNumber = houseNumber + 1
        }

        houseNumber
    }

    private def generateDivisors(number: Int) = {
        (1 to Math.sqrt(number).toInt)
            .filter(number % _ == 0)
            .map(n => n :: (if (number/n != n) List(number/n) else Nil)).flatten
    }
}