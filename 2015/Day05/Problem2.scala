object Problem2 extends App {
    println(args.foldLeft(0)((t, v) => t + (if (isNice(v)) 1 else 0)));

    def isNice(s: String) : Boolean = {
        return hasPalindrome3(s) && hasRepeatingSequence(s);
    }

    def hasRepeatingSequence(s: String): Boolean = {
        for (i <- 0 to s.length - 3) {
            if (s.substring(i + 2).contains(s.substring(i, i + 2))) {
                return true;
            }
        }

        return false;
    }

    def hasPalindrome3(s: String) : Boolean = {
        return s.sliding(3).exists(s => s(0) == s(2));
    }
}

