import java.io.*;
import java.lang.Math;
import java.util.*;
import utils.*;

public class Problems {
    private static long problem1(List<List<Integer>> numbers, List<String> operations) {
        long result = 0;
        for (var i = 0; i < operations.size(); i++) {
            var operation = operations.get(i);
            long acc =  operation.equals("*") ? 1 : 0;
            for (var j = 0; j < numbers.size(); j++) {
                var operand = numbers.get(j).get(i);
                if (operation.equals("*")) {
                    acc *= operand;
                } else if (operation.equals("+")) {
                    acc += operand;
                } else {
                    System.out.println("Unexpected operation: |" + operation + "|");
                }
            }
            result += acc;
        }
        return result;
    }

    private static int getNumber(List<String> numbers, int column) {
        if (column >= numbers.get(0).length()) {
            return 0;
        }
        int n = 0;
        for (var i = 0; i < numbers.size(); i++) {
            char c = numbers.get(i).charAt(column);
            if (c != ' ') {
                n = n * 10 + (c - '0');
            }
        }
        return n;
    }

    private static long problem2(List<String> numbers, String operations) {
        long res = 0;
        long acc = 0;
        char operation = ' ';
        for (var i = 0; i <= operations.length(); i++) {
            var n = getNumber(numbers, i);

            if (n == 0) {
                res += acc;
            } else {
                if (operations.charAt(i) != ' ') {
                    operation = operations.charAt(i);
                    acc = operation == '*' ? 1 : 0;
                }
                if (operation == '*') {
                    acc *= n;
                } else if (operation == '+') {
                    acc += n;
                } else {
                    System.out.println("Unexpected operation: " + operation);
                }
            }
        }
        return res;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        List<List<Integer>> numbers = new ArrayList<>();
        for (var i = 0; i < lines.size() - 1; i++) {
            numbers.add(Arrays.stream(lines.get(i).trim().split("\s+")).map(Integer::parseInt).toList());
        }
        List<String> operations = Arrays.asList(lines.getLast().split("\s+"));
        System.out.println(problem1(numbers, operations));
        System.out.println(problem2(lines.subList(0, lines.size() - 1), lines.getLast()));
    }
}
