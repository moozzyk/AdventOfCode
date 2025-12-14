package utils;

public class DisjointSetNode<T> {
    public final T value;
    public DisjointSetNode<T> parent;
    public int size;

    public DisjointSetNode(T value) {
        this.value = value;
    }
}