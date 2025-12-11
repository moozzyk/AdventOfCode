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

    private static long visit(String current, String target, Map<String, List<String>> graph, Map<String, Long> cache) {
        if (current.equals(target)) {
            return 1;
        }
        if (current.equals("out")) {
            return 0;
        }

        String key = current + target;
        if (!cache.containsKey(key)) {
            long numPaths = graph.get(current).stream()
                .map(n -> visit(n, target, graph, cache))
                .mapToLong(Long::valueOf)
                .sum();
            cache.put(key, numPaths);
        }
        return cache.get(key);
    }

    private static long problem1(Map<String, List<String>> graph) {
        return visit("you", "out", graph, new HashMap<String, Long>());
    }

    private static long problem2(Map<String, List<String>> graph) {
        Map<String, Long> cache = new HashMap<String, Long>();
        long svrfftdacout =
            visit("svr", "fft", graph, cache) *
            visit("fft", "dac", graph, cache) *
            visit("dac", "out", graph, cache);
        long svrdacfftout =
            visit("svr", "dac", graph, cache) *
            visit("dac", "fft", graph, cache) *
            visit("fft", "out", graph, cache);
        return svrfftdacout + svrdacfftout;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        Map<String, List<String>> graph = buildGraph(lines);
        System.out.println(problem1(graph));
        System.out.println(problem2(graph));
    }
}
