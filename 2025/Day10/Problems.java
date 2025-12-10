import java.io.*;
import java.lang.*;
import java.lang.Math;
import java.util.*;
import java.util.regex.*;
import utils.*;


public class Problems {
    private static class BlinkenMachine {
        public final int lights;
        public final List<Integer> buttons;
        public final List<Integer> joltage;

        private static Pattern pattern = Pattern.compile(
            "\\[(?<lights>[.#]+)\\] (?<buttons>.*) \\{(?<joltage>[\\d,]+)\\}"
        );

        private static int parseLights(String s) {
            int lights = 0;
            for (var c: new StringBuilder(s).reverse().toString().toCharArray()) {
                lights = lights << 1;
                if (c == '#') {
                    lights |= 1;
                }
            }

            return lights;
        }

        private static List<Integer> parseButtons(String s) {
            List<Integer> buttons = new ArrayList<>();
            for (var b: s.split(" ")) {
                int configuration = 0;
                var tmp = b.substring(1, b.length() - 1);
                for (var c: tmp.split(",")) {
                    configuration |= (1 << (c.charAt(0) - '0'));
                }
                buttons.add(configuration);
            }
            return buttons;
        }

        private static List<Integer> parseJoltage(String joltage) {
            return Arrays.stream(joltage.split(",")).map(Integer::parseInt).toList();
        }

        public BlinkenMachine(String s) {

            Matcher m = pattern.matcher(s);
            if (m.find()) {
                this.lights = parseLights(m.group("lights"));
                this.buttons = parseButtons(m.group("buttons"));
                this.joltage = parseJoltage(m.group("joltage"));
            } else {
                System.out.println("Not working");
                this.lights = 0;
                this.buttons = null;
                this.joltage = null;
            }
        }
    }

    private static int minPresses(BlinkenMachine machine) {
        Set<Integer> visited = new HashSet<>();
        Queue<Pair<Integer>> q = new LinkedList<>();
        q.add(new Pair<Integer>(0, 0));
        while (!q.isEmpty()) {
            var item = q.remove();
            if (item.second > 20) {
                System.out.println("Error o kurde");
                break;
            }
            if (visited.contains(item.first)) {
                continue;
            }

            if (item.first == machine.lights) {
                return item.second;
            }

            for (var buttons : machine.buttons) {
                q.add(new Pair<Integer>(item.first ^ buttons, item.second + 1));
            }
        }

        return -1;
    }

    private static int problem1(List<BlinkenMachine> machines) {
        return machines.stream().map(Problems::minPresses).mapToInt(Integer::intValue).sum();
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        List<BlinkenMachine> machines = lines.stream().map(BlinkenMachine::new).toList();

        System.out.println(problem1(machines));
    }
}
