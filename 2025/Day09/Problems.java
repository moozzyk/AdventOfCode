import java.io.*;
import java.lang.Math;
import java.util.*;
import utils.*;

public class Problems {
    private static class Edge {
        public final Pair<Integer> t1;
        public final Pair<Integer> t2;

        public Edge(Pair<Integer> t1, Pair<Integer> t2) {
            this.t1 = t1;
            this.t2 = t2;
        }
    }

    private static long getArea(Pair<Integer> t1, Pair<Integer> t2) {
        return ((long)(1 + Math.abs(t1.first - t2.first))) * ((long)(1 + Math.abs(t1.second - t2.second)));
    }

    private static Long problem1(List<Pair<Integer>> tiles) {
        long maxArea = 0;
        for (var i = 0; i < tiles.size(); i++) {
            for (var j = i + 1; j < tiles.size(); j++) {
                maxArea = Math.max(maxArea, getArea(tiles.get(i), tiles.get(j)));
            }
        }
        return maxArea;
    }

    private static List<Edge> buildEdges(List<Pair<Integer>> tiles) {
        List<Edge> edges = new ArrayList<>();
        for (var i = 0; i < tiles.size(); i++) {
            var c = tiles.get(i);
            var n = tiles.get((i + 1) % tiles.size());
            edges.add(new Edge(tiles.get(i), tiles.get((i + 1) % tiles.size())));
        }
        return edges;
    }

    // Makes assumptions and ignores corner cases given the input
    // (e.g., doesn't work on the problem example)
    private static boolean isValidRectangle(Pair<Integer> c1, Pair<Integer> c2, List<Edge> edges) {
        var topLeft = new Pair<Integer>(Math.min(c1.first, c2.first), Math.min(c1.second, c2.second));
        var bottomRight = new Pair<Integer>(Math.max(c1.first, c2.first), Math.max(c1.second, c2.second));
        for (var e: edges) {
            if (!(Math.max(e.t1.first, e.t2.first) <= topLeft.first ||
                Math.min(e.t1.first, e.t2.first) >= bottomRight.first ||
                Math.max(e.t1.second, e.t2.second) <= topLeft.second ||
                Math.min(e.t1.second, e.t2.second) >= bottomRight.second)) {
                return false;
            }
        }

        return true;
    }

    private static Long problem2(List<Pair<Integer>> tiles) {
        long maxArea = 0;
        var edges = buildEdges(tiles);
        for (var i = 0; i < tiles.size(); i++) {
            for (var j = i + 1; j < tiles.size(); j++) {
                var area = getArea(tiles.get(i), tiles.get(j));
                if (area > maxArea && isValidRectangle(tiles.get(i), tiles.get(j), edges)) {
                    maxArea = area;
                }
            }
        }
        return maxArea;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        List<Pair<Integer>> tiles = new ArrayList<>();
        for (var l: lines) {
            var parts = l.split(",");
            tiles.add(new Pair<Integer>(Integer.parseInt(parts[0]), Integer.parseInt(parts[1])));
        }

        System.out.println(problem1(tiles));
        System.out.println(problem2(tiles));
    }
}
