import { readLines, lcm } from "../utils.js";

function createNodes(lines) {
  const regexp = /\w\w\w/g;
  return new Map(
    lines
      .map((l) => l.match(regexp))
      .map(([from, left, right]) => [from, { L: left, R: right }])
  );
}

function getNumSteps(turns, nodes, startNode, endNodes) {
  let currentNode = startNode;
  let turnIdx = 0;
  let steps = 0;
  while (!endNodes.has(currentNode)) {
    currentNode = nodes.get(currentNode)[turns[turnIdx]];
    turnIdx = (turnIdx + 1) % turns.length;
    steps++;
  }
  return steps;
}

function problem1(turns, nodes) {
  return getNumSteps(turns, nodes, "AAA", new Set(["ZZZ"]));
}

function problem2(turns, nodes) {
  const endNodes = new Set(
    [...nodes.values()]
      .map(({ L, R }) => [L, R])
      .flat()
      .filter((n) => n.endsWith("Z"))
  );

  return lcm(
    [...nodes.keys()]
      .filter((n) => n.endsWith("A"))
      .map((startNode) => getNumSteps(turns, nodes, startNode, endNodes))
  );
}

const lines = readLines(process.argv[2]);
const turns = lines[0];
const nodes = createNodes(lines.slice(1));
console.log(problem1(turns, nodes));
console.log(problem2(turns, nodes));
