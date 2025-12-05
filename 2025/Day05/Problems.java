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
    }
}
