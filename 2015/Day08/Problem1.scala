object Problem1 extends App {
    println(countCharDifference(scala.io.Source.fromFile(args(0)).getLines))

    private def countCharDifference(strings: Iterator[String]): Int = {
        strings.foldLeft(0)((t, s) => t + s.length() - getNumChars(s))
    }

    private def getNumChars(input: String, index: Int = 0): Int = {

        if (index == input.length()) {
            return 0;
        }

        if (input(index) == '\"') {
            return getNumChars(input, index + 1);
        }

        if (input(index) != '\\') {
            return 1 + getNumChars(input, index + 1);
        }

        if (input(index + 1) == 'x') {
            return 1 + getNumChars(input, index + 4);
        }
        else {
            return 1 + getNumChars(input, index + 2)
        }
    }
}