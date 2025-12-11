import java.io.*;
import java.lang.Math;
import java.util.*;
import utils.*;

public class Problems {
    private static Map<String, List<String>> buildGraph(List<String> lines) {
        var graph = new HashMap<String, List<String>>();
        for(var l: lines) {
            var entry = l.split(": ");
            var values = entry[1].split(" ");
            graph.put(entry[0], Arrays.asList(values));
        }
        return graph;
    }

    private static int visit(String current, Map<String, List<String>> graph) {
        if (current.equals("out")) {
            return 1;
        }
        return graph.get(current).stream().map(n -> visit(n, graph)).mapToInt(Integer::valueOf).sum();
    }

    private static int problem1(Map<String, List<String>> graph) {
        return visit("you", graph);
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        Map<String, List<String>> graph = buildGraph(lines);
        System.out.println(problem1(graph));
    }
}
