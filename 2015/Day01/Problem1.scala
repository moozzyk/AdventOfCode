object Problem1 extends App {
    println(args(0).foldLeft(0)((s, v) => s + (if (v == '(') 1 else -1)))
}