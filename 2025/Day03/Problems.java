import java.io.*;
import java.lang.Math;
import java.util.*;

public class Problems {
    private static int maxJoltage(int[] bank) {
        var result = -1;
        for (int i = 0; i < bank.length - 1; i++) {
            for (int j = i + 1; j < bank.length; j++) {
                result = Math.max(result, bank[i] * 10 + bank[j]);
            }
        }
        return result;
    }

    private static int problem1(List<int[]> batteries) {
        return batteries.stream().map(Problems::maxJoltage).mapToInt(Integer::intValue).sum();
    }

    public static void main(String[] args) throws IOException {
        List<int[]> batteries = new ArrayList<>();
        try (var br = new BufferedReader(new FileReader(args[0]))) {
            String line;
            while ((line = br.readLine()) != null) {
                batteries.add(line.chars().map(c -> c - '0').toArray());
            }
        }

        System.out.println(problem1(batteries));
    }
}
