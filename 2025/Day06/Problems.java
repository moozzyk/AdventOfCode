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

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        List<List<Integer>> numbers = new ArrayList<>();
        for (var i = 0; i < lines.size() - 1; i++) {
            numbers.add(Arrays.stream(lines.get(i).trim().split("\s+")).map(Integer::parseInt).toList());
        }
        List<String> operations = Arrays.asList(lines.getLast().split("\s+"));
        System.out.println(problem1(numbers, operations));
    }
}
