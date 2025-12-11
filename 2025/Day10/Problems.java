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

    private static void updateJoltage(Integer buttons, List<Integer> joltage, int delta) {
        for (int i = 0; i < 15; i++) {
            if ((buttons & (1 << i)) > 0) {
                joltage.set(i, joltage.get(i) + delta);
            }
        }
    }

    private static int getMinJoltageIdx(List<Integer> joltage) {
        int minIdx = -1;
        for (var i = 0; i < joltage.size(); i++) {
            if (joltage.get(i) > 0 && (minIdx == -1 || joltage.get(minIdx) > joltage.get(i))) {
                minIdx = i;
            }
        }
        return minIdx;
    }

    private static int minPresses(List<Integer> buttons, List<Integer> joltage, int numPresses, boolean[] usedButtons, int[] minValue) {
        var c = 0; for (var x: usedButtons) if (x) c++;
        if (numPresses >= minValue[0]) {
            return Integer.MAX_VALUE;
        }
        boolean allZeros = true;
        for(var j: joltage) {
            if (j < 0) { return Integer.MAX_VALUE; }
            if (j > 0) { allZeros = false; }
        }
        if (allZeros) {
            minValue[0] = Math.min(minValue[0], numPresses);
            return numPresses;
        }

        int availableButtons = 0;
        for (var i = 0; i < buttons.size(); i++) {
            if (!usedButtons[i]) {
                availableButtons = availableButtons | buttons.get(i);
            }
        }

        for (var i = 0; i < joltage.size(); i++) {
            if (joltage.get(i) > 0 && (availableButtons & (1 << i)) == 0) {
                // System.out.println("Hopeless");
                return Integer.MAX_VALUE;
            }

            if (numPresses + joltage.get(i) >= minValue[0]) {
                return Integer.MAX_VALUE;
            }
        }

        var minJoltageIdx = getMinJoltageIdx(joltage);
        var buttonIdx = -1;
        var bitCount = 0;
        List<Integer> candidateButtons = new ArrayList<>();
        for (var i = 0; i < buttons.size(); i++) {
            if (!usedButtons[i] && (buttons.get(i) & (1 << minJoltageIdx)) != 0) {
                var t = Integer.bitCount(buttons.get(i));
                if (t > bitCount) {
                    buttonIdx = i;
                    bitCount = t;
                }
                candidateButtons.add(i);
            }
        }
        if (candidateButtons.size() == 0) {
            return Integer.MAX_VALUE;
        }
        var minButtonPresses = Integer.MAX_VALUE;
        var maxDelta = joltage.get(minJoltageIdx);
        usedButtons[buttonIdx] = true;
        if (candidateButtons.size() == 1) {
            updateJoltage(buttons.get(buttonIdx), joltage, -maxDelta);
            minButtonPresses = minPresses(buttons, joltage, numPresses + maxDelta, usedButtons, minValue);
            updateJoltage(buttons.get(buttonIdx), joltage, maxDelta);
        } else {
            for (var delta = maxDelta; delta >= 0; delta--) {
                updateJoltage(buttons.get(buttonIdx), joltage, -delta);
                var presses = minPresses(buttons, joltage, numPresses + delta, usedButtons, minValue);
                minButtonPresses = Math.min(minButtonPresses, presses);
                updateJoltage(buttons.get(buttonIdx), joltage, delta);
            }
        }
        usedButtons[buttonIdx] = false;
        return minButtonPresses;
    }

    private static boolean simplify(int[] matrix, List<Integer> joltage) {
        for (var i = 0; i < matrix.length; i++) {
            for (var j = 0; j < matrix.length; j++) {
                if (matrix[i] < matrix[j] && (matrix[i] & matrix[j]) == matrix[i]) {
                    matrix[j] = matrix[j] - matrix[i];
                    var newJoltage = joltage.get(j) - joltage.get(i);
                    joltage.set(j, newJoltage);
                    return true;
                }
            }
        }
        return false;
    }

    private static void debugDump(int[] matrix, List<Integer> joltage) {
        for (var i = 0; i < matrix.length; i++) {
            var s = String.format("%12s", Integer.toBinaryString(matrix[i])).replace(' ', '0');
            System.out.printf("%s %d\n", s, joltage.get(i));
        }
        System.out.println();
    }

    private static int optimize(List<Integer> joltage, List<Integer> buttons) {
        var m = new int[joltage.size()];
        for (var i = 0; i < buttons.size(); i++) {
            for (var j = 0; j < joltage.size(); j++) {
                m[j] = m[j] << 1;
                var t = (buttons.get(i) & (1 << j)) != 0 ? 1 : 0;
                m[j] |= t;
            }
        }

        // debugDump(m, joltage);
        while(simplify(m, joltage)) {
            // debugDump(m, joltage);
        }
        // debugDump(m, joltage);

        for (var i = buttons.size() - 1; i >= 0; i--) {
            int button = 0;
            for (var j = joltage.size() - 1; j >= 0; j--) {
                button = button << 1;
                button |= ((m[j] & (1 << i)) != 0 ? 1 : 0);
            }
            buttons.set(i, button);
        }
        return 0;
    }

    private static int solve(BlinkenMachine m) {
        System.out.print("Machine buttons: " + m.buttons + ", Joltage: " + m.joltage);
        var joltage = new ArrayList<>(m.joltage);
        var buttons = new ArrayList<>(m.buttons);
        optimize(joltage, buttons);
        var presses = minPresses(buttons, joltage, 0, new boolean[buttons.size()], new int[] {Integer.MAX_VALUE});
        System.out.printf(", Num presses: %d\n", presses);
        return presses;
    }

    private static int problem2(List<BlinkenMachine> machines) {
        System.out.println("Problem2: patience test");
        return machines.stream().map(Problems::solve).mapToInt(Integer::valueOf).sum();
    }

    public static void main(String[] args) throws IOException {
        List<String> lines =  FileUtils.readLines(args[0]);
        List<BlinkenMachine> machines = lines.stream().map(BlinkenMachine::new).toList();

        System.out.println(problem1(machines));
        System.out.println(problem2(machines));
    }
}
