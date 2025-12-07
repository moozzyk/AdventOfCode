import java.io.*;
import java.lang.Math;
import java.util.*;
import utils.*;

public class Problems {
    private static int findStart(String s) {
        for (var i = 0; i < s.length(); i++) {
            if (s.charAt(i) == 'S') {
                return i;
            }
        }
        System.out.println("Cannot find the starting point");
        return -1;
    }
    private static int problem1(List<String> manifold) {
        boolean[][] beams = new boolean[manifold.size()][manifold.get(0).length()];
        beams[0][findStart(manifold.get(0))] = true;
        int numSplits = 0;

        for (var i = 1; i < manifold.size(); i++) {
            var row = manifold.get(i);
            for (var j = 0; j < row.length(); j++) {
                if (row.charAt(j) == '^' && beams[i-1][j]) {
                    if (beams[i-1][j]) {
                        numSplits++;
                    }
                    beams[i][j-1] = beams[i][j+1] = true;
                } else {
                    beams[i][j] = beams[i][j] || beams[i-1][j];
                }
            }
        }
        return numSplits;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        System.out.println(problem1(lines));
    }
}
