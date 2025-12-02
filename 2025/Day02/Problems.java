import java.io.*;
import java.lang.Math;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.LongStream;

public class Problems {
    private static boolean isInvalidId(long n) {
        var s = Long.toString(n);
        return s.substring(0, s.length() / 2).equals(s.substring(s.length() / 2));
    }

    private static long findInvalidIdSumInRange(long start, long end) {
        long invalidIdSum = 0;
        for (var n = start; n <= end; n++) {
            if (isInvalidId(n)) {
                invalidIdSum += n;
            }
        }

        return invalidIdSum;
    }

    private static long problem1(List<long[]> ranges) {
        var invalidIdSum = 0L;
        for (var range: ranges) {
            invalidIdSum += findInvalidIdSumInRange(range[0], range[1]);
        }
        return invalidIdSum;
    }

    public static void main(String[] args) throws IOException {
        var input = Files.readString(Paths.get(args[0]), StandardCharsets.UTF_8);
        var ranges =
            Arrays.stream(input.split(","))
                .map(s -> Arrays.stream(s.split("-")).mapToLong(Long::parseLong).toArray())
                .toList();

        System.out.println(problem1(ranges));
    }
}
