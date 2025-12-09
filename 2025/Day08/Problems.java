import java.io.*;
import java.lang.Math;
import java.util.*;
import utils.*;

public class Problems {
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

    private static class DisjointSetNode<T> {
        final T value;
        T parent;
        int size;

        public DisjointSetNode(T value) {
            this.value = value;
        }
    }

    public static class DisjointSet<T> {
        public final Map<T, DisjointSetNode<T>> nodes = new HashMap<>();

        private DisjointSetNode<T> findRoot(DisjointSetNode<T> n) {
            while (n.parent != null && n.parent != n) {
                n = n.parent;
            }
            return n;
        }

        private DisjointSet<T> getDisjointNode(T v) {
            var n = nodes.get(v1);
            if (n == null) {
                n = new DisjointSetNode(v);
                nodes.put(v, n);
            }
            return n;
        }

        public void add(T v1, T v2) {
            var n1 = getDisjointNode(v1);
            var r1 = findRoot(v1);

            var n2 = getDisjointNode(v2);
            var r2 = findRoot(v2);

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
        DistjointSet<Point3d> set = new DisjointSet<Point3d>();
        for (var i = 0; i < 10; i++) {
            var d = distances.get(i);
            set.add(d.p1, d.p2);
        }

        for (var n: set.values()) {
            System.out.printf("%s %d %s\n", n.value.toString(), n.size, n.parent.toString());
        }

        return 0;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        List<Point3d> points = new ArrayList<>();
        for (var l : lines) {
            var t = l.split(",");
            System.out.println(Arrays.toString(t));
            points.add(new Point3d(Integer.parseInt(t[0]), Integer.parseInt(t[1]), Integer.parseInt(t[2])));
        }

        System.out.println(problem1(points));

        // points.forEach(System.out::println);
        // System.out.println(distance(points.get(1), points.get(2)));
    }
}
