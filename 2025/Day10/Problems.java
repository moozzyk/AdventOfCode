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

    private static int minPresses(BlinkenMachine machine, int buttonIdx, int state, int presses) {
        if (state == machine.lights) {
            return presses;
        }

        if (buttonIdx  == machine.buttons.size()) {
            return Integer.MAX_VALUE;
        }

        return Math.min(
            minPresses(machine, buttonIdx + 1, state, presses),
            minPresses(machine, buttonIdx + 1, state ^ machine.buttons.get(buttonIdx), presses + 1));
    }

    private static int problem1(List<BlinkenMachine> machines) {
        return machines.stream().map(m -> minPresses(m, 0, 0, 0)).mapToInt(Integer::intValue).sum();
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        List<BlinkenMachine> machines = lines.stream().map(BlinkenMachine::new).toList();

        System.out.println(problem1(machines));
    }
}
