import java.io.*;
import java.lang.Math;
import java.util.*;
import utils.*;

public class Problems {
    private static Long problem1(List<Pair<Long>> tiles) {
        long maxArea = 0;
        for (var i = 0; i < tiles.size(); i++) {
            for (var j = i + 1; j < tiles.size(); j++) {
                var t1 = tiles.get(i);
                var t2 = tiles.get(j);
                long area = (1 + Math.abs(t1.first - t2.first)) * (1 + Math.abs(t1.second - t2.second));
                maxArea = Math.max(maxArea, area);
            }
        }
        return maxArea;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        List<Pair<Long>> tiles = new ArrayList<>();
        for (var l: lines) {
            var parts = l.split(",");
            tiles.add(new Pair<Long>(Long.parseLong(parts[0]), Long.parseLong(parts[1])));
        }

        System.out.println(problem1(tiles));
    }
}
