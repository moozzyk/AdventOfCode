object Problem2 extends App {
    println(Helper.getHouseNumber(isRightHouse))

    private def isRightHouse(divisors: Seq[Int], houseNumber: Int): Boolean = {
        divisors.filter(d => d > houseNumber - 50 * d).fold(0)((t, v) => t + 11 * v) > args(0).toInt
    }
}