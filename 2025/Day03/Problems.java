import java.io.*;
import java.lang.Math;
import java.util.*;

public class Problems {
    private static long maxJoltage(int[] bank, int size) {
        Stack<Integer> stack = new Stack<>();
        for (int i = 0; i < bank.length; i++) {
            while (!stack.empty() && bank[i] > stack.peek() && stack.size() + bank.length - i > size) {
                stack.pop();
            }
            if (stack.size() < size) {
                stack.push(bank[i]);
            }
        }

        var result = 0L;
        var order = 1L;
        while (!stack.empty()) {
            result += order * stack.peek();
            stack.pop();
            order *= 10;
        }
        return result;
    }

    private static long solve(List<int[]> batteries, int size) {
        return batteries.stream().map(bank -> maxJoltage(bank, size)).mapToLong(Long::longValue).sum();
    }

    private static long problem1(List<int[]> batteries) {
        return solve(batteries, 2);
    }

    private static long problem2(List<int[]> batteries) {
        return solve(batteries, 12);
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
        System.out.println(problem2(batteries));
    }
}
