object Problem2 extends App {
    println(args(0).scanLeft(0)((s, v) => s + (if (v == '(') 1 else -1)).takeWhile(_ >= 0).length)
}