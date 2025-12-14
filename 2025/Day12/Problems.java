import java.io.*;
import java.lang.*;
import java.util.*;
import java.util.stream.*;
import utils.*;

public class Problems {
    private static List<Integer> shapeTiles;

    private static boolean fitNoGaps(int width, int height, int[] presents) {
        var minimuRequiredArea = 0;
        for (var i = 0; i < presents.length; i++) {
            minimuRequiredArea += presents[i] * shapeTiles.get(i);
        }
        return minimuRequiredArea <= width * height;
    }

    private static boolean fitNoStacking(int width, int height, int[] presents) {
        int numPresentsPossible = (width / 3) * (height / 3);
        int numPresents = Arrays.stream(presents).sum();
        return numPresentsPossible >= numPresents;
    }

    private static boolean solve(int width, int height, int[] presents) throws UnsupportedOperationException {
        if (!fitNoGaps(width, height, presents)) {
            return false;
        }
        if (fitNoStacking(width, height, presents)) {
            return true;
        }
        throw new UnsupportedOperationException("Too complex.");
    }

    private static int problem1(List<String> lines) {
        int result = 0;
        for (var line: lines) {
            var width = Integer.parseInt(line.substring(0, 2));
            var height = Integer.parseInt(line.substring(3, 5));
            int[] presents = Arrays.stream(line.substring(7, 24).split(" "))
                .mapToInt(Integer::parseInt).toArray();
            result += solve(width, height, presents) ? 1 : 0;
        }
        return result;
    }

    private static List<Integer> getNumTiles(List<String> lines) {
        List<char[][]> shapes = new ArrayList<>();
        List<Integer> numTiles = new ArrayList<>();
        for (var i = 1; i < lines.size(); i++) {
            int tmp = 0;
            for (var j = 0; j < 3; j++, i++) {
                for (var c: lines.get(i).toCharArray()) {
                    tmp += c == '#' ? 1 : 0;
                }
            }
            numTiles.add(tmp);
            i += 1;
        }
        return numTiles;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        shapeTiles = getNumTiles(lines.stream().limit(30).toList());
        System.out.println(problem1(lines.stream().skip(30).toList()));
    }
}
