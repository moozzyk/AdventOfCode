object Problem1 extends App {
    println(Miner.mine(args(0), h => h.startsWith("00000")));
}