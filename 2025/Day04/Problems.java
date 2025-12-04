import java.io.*;
import java.lang.Math;
import java.util.*;

public class Problems {
    public static class Pair<T> {
        public final T first;
        public final T second;

        public Pair(T first, T second) {
            this.first = first;
            this.second = second;
        }
    }

    private static boolean isAccessible(char[][] map, int row, int col) {
        int numRolls = 0;
        for (int r = row - 1; r < row + 2; r++) {
            for (int c = col - 1; c < col + 2; c++) {
                if (r == row && c == col) continue;
                if (r < 0 || c < 0 || r >= map.length || c >= map[0].length) continue;
                if (map[r][c] == '@') {
                    numRolls++;
                }
            }
        }
        return numRolls < 4;
    }

    private static List<Pair<Integer>> findAccessiblePaper(char[][] map) {
        List<Pair<Integer>> accessiblePaper = new ArrayList<>();
        for (var row = 0; row < map.length; row++) {
            for (var col = 0; col < map[row].length; col++) {
                if (map[row][col] == '@' && isAccessible(map, row, col)) {
                    accessiblePaper.add(new Pair<Integer>(row, col));
                }
            }
        }
        return accessiblePaper;
    }

    private static int problem1(char[][] map) {
        return findAccessiblePaper(map).size();
    }

    private static int problem2(char[][] map) {
        int result = 0;
        while (true) {
            var accessiblePaper = findAccessiblePaper(map);
            if (accessiblePaper.size() == 0) {
                break;
            }
            result += accessiblePaper.size();
            for (var p: accessiblePaper) {
                map[p.first][p.second] = '.';
            }
        }
        return result;
    }

    public static void main(String[] args) throws IOException {
        List<char[]> input = new ArrayList<>();
        try (var br = new BufferedReader(new FileReader(args[0]))) {
            String line;
            while ((line = br.readLine()) != null) {
                input.add(line.toCharArray());
            }
        }
        char[][] map = new char[input.size()][];
        for (var i = 0; i < input.size(); i++) {
            map[i] = input.get(i);
        }

        System.out.println(problem1(map));
        System.out.println(problem2(map));
    }
}
