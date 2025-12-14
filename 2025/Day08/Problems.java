import java.io.*;
import java.lang.Math;
import java.util.*;
import utils.*;

public class Problems {
    private static class DisjointSetNode<T> {
        final T value;
        DisjointSetNode<T> parent;
        int size;

        public DisjointSetNode(T value) {
            this.value = value;
        }
    }

    public static class DisjointSet<T> {
        public final Map<T, DisjointSetNode<T>> nodes = new HashMap<>();

        public DisjointSetNode<T> findRoot(DisjointSetNode<T> n) {
            while (n.parent != null && n.parent != n) {
                n = n.parent;
            }
            return n;
        }

        private DisjointSetNode<T> getDisjointNode(T v) {
            var n = nodes.get(v);
            if (n == null) {
                n = new DisjointSetNode(v);
                nodes.put(v, n);
            }
            return n;
        }

        public void add(T v1, T v2) {
            var n1 = getDisjointNode(v1);
            var r1 = findRoot(n1);

            var n2 = getDisjointNode(v2);
            var r2 = findRoot(n2);

            if (r1 == r2) {
                return;
            }

            if (r1.parent == null && r2.parent == null) {
                r1.parent = r2.parent = r1;
                r1.size = 2;
                r2.size = 1;
                return;
            }

            if (r1.parent == null) {
                r1.parent = r2;
                r1.size = 1;
                r2.size += 1;
                return;
            }

            if (r2.parent == null) {
                r2.parent = r1;
                r2.size = 1;
                r1.size += 1;
                return;
            }

            r2.parent = r1;
            r1.size += r2.size;
        }
    }

    private static class Distance {
        public final double distance;
        public final Point3d p1;
        public final Point3d p2;

        public Distance(double distance, Point3d p1, Point3d p2) {
            this.distance = distance;
            this.p1 = p1;
            this.p2 = p2;
        }
    }

    private static double computeDistance(Point3d p1, Point3d p2) {
        return Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2) + Math.pow(p1.z - p2.z, 2));
    }

    private static List<Distance> getSortedDistances(List<Point3d> points) {
        List<Distance> distances = new ArrayList<>();
        for (int i = 0; i < points.size(); i++) {
            for (int j = i + 1; j < points.size(); j++) {
                var p1 = points.get(i);
                var p2 = points.get(j);
                if (p1 != p2) {
                    distances.add(new Distance(computeDistance(p1, p2), p1, p2));
                }
            }
        }
        distances.sort((p1, p2) -> Double.compare(p1.distance, p2.distance));
        return distances;
    }

    private static int problem1(List<Point3d> points) {
        var distances = getSortedDistances(points);
        var set = new DisjointSet<Point3d>();
        for (var i = 0; i < 1000; i++) {
            var d = distances.get(i);
            set.add(d.p1, d.p2);
        }

        return set.nodes.values().stream()
            .filter(n -> n == n.parent)
            .map(n -> n.size)
            .sorted(Comparator.reverseOrder())
            .limit(3)
            .reduce(1, (a, b) -> a * b);
    }

    private static boolean hasOneRoot(DisjointSet<Point3d> set) {
        DisjointSetNode<Point3d> root = null;
        for (var n: set.nodes.values()) {
            var currentRoot = set.findRoot(n);
            if (root == null) {
                root = currentRoot;
            } else if (currentRoot != root) {
                return false;
            }
        }
        return true;
    }

    private static long problem2(List<Point3d> points) {
        var distances = getSortedDistances(points);
        var set = new DisjointSet<Point3d>();
        for (var d: distances) {
            set.add(d.p1, d.p2);
            System.out.printf("%s %s %d\n", d.p1, d.p2, set.nodes.size());
            if (set.nodes.size() == points.size() && hasOneRoot(set)) {
                return (long)d.p1.x * (long)d.p2.x;
            }
        }
        return -1;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        List<Point3d> points = new ArrayList<>();
        for (var l : lines) {
            var t = l.split(",");
            points.add(new Point3d(Integer.parseInt(t[0]), Integer.parseInt(t[1]), Integer.parseInt(t[2])));
        }
        System.out.println(problem1(points));
        System.out.println(problem2(points));
    }
}
