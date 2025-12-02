import java.io.*;
import java.lang.Math;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Function;
import java.util.stream.IntStream;
import java.util.stream.LongStream;

public class Problems {
    private static boolean isInvalidId(long n) {
        var s = Long.toString(n);
        return s.substring(0, s.length() / 2).equals(s.substring(s.length() / 2));
    }

    private static boolean isSeqRepeating(String s, int seqLength) {
        var seq = s.substring(0, seqLength);
        for (var i = 0; i < s.length(); i += seqLength) {
            if (!s.substring(i, Math.min(i + seqLength, s.length())).equals(seq)) {
                return false;
            }
        }
        return true;
    }

    private static boolean isInvalidIdExtended(long n) {
        var s = Long.toString(n);
        return IntStream.rangeClosed(1, s.length() / 2).anyMatch(seqLength -> isSeqRepeating(s, seqLength));
    }

    private static long findInvalidIdSumInRange(long start, long end, Function<Long, Boolean> invalidIdFn) {
        return LongStream.rangeClosed(start, end).filter(n -> invalidIdFn.apply(n)).sum();
    }

    private static long solve(List<long[]> ranges, Function<Long, Boolean> invalidIdFn) {
        return ranges.stream()
            .map(r -> findInvalidIdSumInRange(r[0], r[1], invalidIdFn))
            .mapToLong(Long::longValue)
            .sum();
    }

    private static long problem1(List<long[]> ranges) {
        return solve(ranges, Problems::isInvalidId);
    }

    private static long problem2(List<long[]> ranges) {
        return solve(ranges, Problems::isInvalidIdExtended);
    }

    public static void main(String[] args) throws IOException {
        var input = Files.readString(Paths.get(args[0]), StandardCharsets.UTF_8);
        var ranges =
            Arrays.stream(input.split(","))
                .map(s -> Arrays.stream(s.split("-")).mapToLong(Long::parseLong).toArray())
                .toList();

        System.out.println(problem1(ranges));
        System.out.println(problem2(ranges));
    }
}
