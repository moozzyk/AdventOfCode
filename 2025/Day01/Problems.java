import java.io.*;
import java.util.*;

public class Problems {
    private static int problem1(List<String> rotations) {
        var curr = 50;
        var res = 0;
        for (var rotation: rotations) {
            var steps = Integer.parseInt(rotation.substring(1));
            if (rotation.charAt(0) == 'L') {
                curr = curr + 100 - steps;
            } else {
                curr += steps;
            }
            curr %= 100;
            if (curr == 0) {
                res++;
            }
        }
        return res;
    }

    public static void main(String[] args) throws IOException {
        var lines = new ArrayList<String>();
        try (var br = new BufferedReader(new FileReader(args[0]))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        }

        System.out.println(problem1(lines));
    }
}
