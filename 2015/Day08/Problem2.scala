object Problem2 extends App {
    println(countCharDifference(scala.io.Source.fromFile(args(0)).getLines))

    private def countCharDifference(strings: Iterator[String]): Int = {
        strings.foldLeft(0)((t, s) => t + encode(s).length - s.length())
    }

    def encode(input: String): String = {
        quote(
            input.flatMap {
                case '\\' => "\\\\"
                case '"' => "\\\""
                case c => s"$c"
            });
    }

   private def quote(input: String): String = {
        return "\"" + input + "\"";
    }
}