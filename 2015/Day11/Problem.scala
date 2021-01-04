//

object Problem1 extends App {

    val pwd = args(0).toArray;
    do {
        next(pwd)
    } while(!isValid(pwd))

    println(pwd.mkString)

    private def isValid(input: Array[Char]): Boolean = {
        return !hasIOL(input) && hasIncreasingTriplet(input) && uniqueNonOverlappingPairs(input) > 1;
    }

    private def hasIOL(input: Array[Char]): Boolean = {
        return input.contains('i') || input.contains('o') || input.contains('l');
    }

    private def hasIncreasingTriplet(input: Array[Char]): Boolean = {
        return input.sliding(3).exists(s => s(0) + 1 == s(1) && s(2) - 1 == s(1));
    }

    private def uniqueNonOverlappingPairs(input: Array[Char]): Int = {
        nonOverlappingPairs(input).map(i => input(i)).distinct.size;
    }

    private def nonOverlappingPairs(input: Array[Char]): Array[Int] = {
        var i = 0;
        val pairIndices = new scala.collection.mutable.ArrayBuffer[Int]();

        while (i < input.size - 1) {
            if (input(i) == input(i + 1)) {
                pairIndices.append(i);
                i = i + 1;
            }

            i = i + 1;
        }

        pairIndices.toArray;
    }

    private def next(input: Array[Char]): Array[Char] = {
        next(input, input.size - 1);
        return input;
    }

    private def next(input: Array[Char], index: Int):Unit = {
        if (index >= 0) {
            if (input(index) == 'z') {
                input(index) = 'a';
                next(input, index - 1);
            }
            else {
                val inc = if (input(index) == 'h' || input(index) == 'k' || input(index) == 'n') 2 else 1;
                input(index) = (input(index) + inc).toChar;
            }
        }
    }
}