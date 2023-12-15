import { readLines, sum } from "../utils.js";

function computeHash(s) {
  return s
    .split("")
    .reduce((acc, s) => (17 * (acc + s.charCodeAt(0))) % 256, 0);
}

function problem1(steps) {
  return sum(steps.map(computeHash));
}

const steps = readLines(process.argv[2])[0].split(",");
console.log(problem1(steps));
