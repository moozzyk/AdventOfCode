package utils;

import java.util.*;

public class DisjointSet<T> {
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
            n = new DisjointSetNode<T>(v);
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
