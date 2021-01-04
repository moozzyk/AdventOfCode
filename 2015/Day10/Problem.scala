import scala.language.postfixOps

object Problem1 extends App {

    var s = args(0);
    for (count <- 0 until args(1).toInt) {
        s = expand(s)
    }

    println(s"${s} ${s.length}")

    private def expand(s: String): String = {
        var c = s(0);
        var count = 0;
        var sb = new StringBuilder();

        for (i <- 0 until s.length) {
            if (s(i) == c) {
                count = count + 1;
            }
            else {
                sb.append(count.toString).append(c);
                count = 1;
                c = s(i);
            }
        }
        sb.append(count.toString).append(c);

        sb.toString();
    }
}