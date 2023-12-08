import { readLines, isDigit } from "../utils.js";

function createNodes(lines) {
  const regexp = /\w\w\w/g;
  return new Map(
    lines
      .map((l) => l.match(regexp))
      .map(([from, left, right]) => [from, { L: left, R: right }])
  );
}

function problem1(turns, nodes) {
  let currentNode = "AAA";
  let turnIdx = 0;
  let steps = 0;
  while (currentNode != "ZZZ") {
    currentNode = nodes.get(currentNode)[turns[turnIdx]];
    turnIdx = (turnIdx + 1) % turns.length;
    steps++;
  }
  return steps;
}

const lines = readLines(process.argv[2]);
const turns = lines[0];
const nodes = createNodes(lines.slice(1));
console.log(problem1(turns, nodes));
