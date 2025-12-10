package utils;

import java.util.Objects;

public class Pair<T> {
    public final T first;
    public final T second;

    public Pair(T first, T second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Pair<T>other = (Pair<T>)o;
        return this.first == other.first && this.second == other.second;
    }

    @Override
    public int hashCode() {
        return Objects.hash(first, second);
    }

    @Override
    public String toString() {
        return "(" + first + ", " + second + ")";
    }
}