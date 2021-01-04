object Problem2 extends App {
    println(Miner.mine(args(0), h => h.startsWith("000000")));
}