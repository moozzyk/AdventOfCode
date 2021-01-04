object Problem1 extends App {
    println(Helper.getHouseNumber(
        (divisors, houseNumber) => divisors.foldLeft(0)((t, v) => t + v * 10) >= args(0).toInt))
}