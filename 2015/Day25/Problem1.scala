object Problem1 extends App {

    println((2 to getIndex(args(0).toInt - 1, args(1).toInt - 1))
        .foldLeft(20151125L)((t, v) => (t * 252533) % 33554393))

    private def getIndex(row: Int, col:Int): Int = {

        val rowStartingVal = 1 + (row) * (row + 1) / 2;
        rowStartingVal + ((row + col + 1) * (row + col + 2) - (row + 1) * (row + 2)) / 2
    }
}