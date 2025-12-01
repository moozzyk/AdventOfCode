import java.io.*;
import java.lang.Math;
import java.util.*;

public class Problems {
    private static int problem1(List<Integer> rotations) {
        var curr = 50;
        var res = 0;
        for (var rotation: rotations) {
            curr += rotation;
            if (curr % 100 == 0) {
                res++;
            }
        }
        return res;
    }

    private static int problem2slow(List<Integer> rotations) {
        var curr = 50;
        var res = 0;
        for (var rotation: rotations) {
            for (int i = 0; i < Math.abs(rotation); i++) {
                if (rotation < 0) {
                    curr -= 1;
                } else {
                    curr += 1;
                }
                curr += 100;
                curr %= 100;
                if (curr == 0) {
                    res++;
                }
            }
        }
        return res;
    }

    private static int problem2(List<Integer> rotations) {
        var curr = 50;
        var res = 0;
        for (var rotation: rotations) {
            var prev = curr;
            curr += rotation;
            res += Math.abs(curr/100);
            if (curr == 0 || (prev < 0 && curr > 0) || (prev > 0 && curr < 0)) {
                res++;
            }
            curr %= 100;
        }
        return res;
    }

    public static void main(String[] args) throws IOException {
        var rotations = new ArrayList<Integer>();
        try (var br = new BufferedReader(new FileReader(args[0]))) {
            String line;
            while ((line = br.readLine()) != null) {
                var steps = Integer.parseInt(line.substring(1));
                if (line.charAt(0) == 'L') {
                    rotations.add(-steps);
                } else {
                    rotations.add(steps);
                }
            }
        }

        System.out.println(problem1(rotations));
        System.out.println(problem2(rotations));
        // System.out.println(problem2slow(rotations));
    }
}
