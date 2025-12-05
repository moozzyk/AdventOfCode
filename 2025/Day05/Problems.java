import java.io.*;
import java.lang.Math;
import java.util.*;
import utils.*;

public class Problems {
    private static int problem1(List<Pair<Long>> ranges, List<Long> ingredients) {
        var result = 0;
        for (var ingredient: ingredients) {
            for (var range: ranges) {
                if (ingredient >= range.first && ingredient <= range.second) {
                    result++;
                    break;
                }
            }
        }
        return result;
    }

    private static List<Pair<Long>> mergeIntervals(List<Pair<Long>> ranges) {
        ranges.sort((r1, r2) -> r1.first.compareTo(r2.first));

        List<Pair<Long>> mergedIntervals = new ArrayList<>();
        mergedIntervals.add(ranges.get(0));
        for (var r: ranges) {
            if (r.second <= mergedIntervals.getLast().second) {
                continue;
            }
            if (r.first > mergedIntervals.getLast().second + 1) {
                mergedIntervals.add(r);
            } else {
                var prev = mergedIntervals.removeLast();
                mergedIntervals.add(new Pair<Long>(prev.first, r.second));
            }
        }
        return mergedIntervals;
    }

    private static long problem2(List<Pair<Long>> ranges) {
        return mergeIntervals(ranges).stream()
            .map(i -> 1 + i.second - i.first)
            .mapToLong(Long::valueOf)
            .sum();
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        List<Pair<Long>> ranges = new ArrayList<>();
        List<Long> ingredients = new ArrayList<>();
        boolean shouldReadIngredients = false;
        for (var l: lines) {
            if (l.length() == 0) {
                shouldReadIngredients = true;
                continue;
            }
            if (shouldReadIngredients) {
                ingredients.add(Long.parseLong(l));
            } else {
                var range = l.split("-");
                ranges.add(new Pair<Long>(Long.parseLong(range[0]), Long.parseLong(range[1])));
            }
        }

        System.out.println(problem1(ranges, ingredients));
        System.out.println(problem2(ranges));
    }
}
