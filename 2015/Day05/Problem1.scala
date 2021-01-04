object Problem1 extends App {
    println(args.foldLeft(0)((t, v) => t + (if (isNice(v)) 1 else 0)));

    def isNice(s: String) : Boolean = {
        return countVowels(s) >= 3 && hasTwoConsecutive(s) && !containsBadStrings(s);
    }

    def countVowels(s: String) : Int = {
        return s.foldLeft(0)((t, v) => { t + (if ("aeiou".contains(v)) 1 else 0) });
    }

    def hasTwoConsecutive(s: String) : Boolean = {
        return s.sliding(2).exists(s => s(0) == s(1));
    }

    def containsBadStrings(s: String) : Boolean = {
        s.contains("ab") || s.contains("cd") || s.contains("pq") || s.contains("xy");
    }
}