import { readLines, isDigit } from "../utils.js";

function parse(lines) {
  const times = lines[0].split(":")[1].match(/\d+/g).map(Number);
  const distances = lines[1].split(":")[1].match(/\d+/g).map(Number);

  return times.map((_, i) => ({ time: times[i], distance: distances[i] }));
}

function waysToWin({ time, distance }) {
  let ways = 0;
  for (let i = 0; i < time; i++) {
    const speed = i;
    const timeRemaining = time - i;
    if (timeRemaining * speed > distance) {
      ways++;
    }
  }
  return ways;
}

function problem1(lines) {
  const races = parse(lines);
  return races.map(waysToWin).reduce((acc, n) => n * acc, 1);
}

const lines = readLines(process.argv[2]);
console.log(problem1(lines));
