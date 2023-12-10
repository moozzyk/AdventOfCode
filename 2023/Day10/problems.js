import { readLines, isDigit } from "../utils.js";

function findStart(pipes) {
  for (let row = 0; row < pipes.length; row++) {
    const col = pipes[row].indexOf("S");
    if (col >= 0) return { row, col };
  }
  throw new Error("Can't find starting point");
}

const UP = 0;
const RIGHT = 1;
const DOWN = 2;
const LEFT = 3;

const directions = [
  [-1, 0],
  [0, 1],
  [1, 0],
  [0, -1],
];

const X = [];
const u = ["7", "|", "F", "S"];
const d = ["L", "|", "J", "S"];
const l = ["L", "-", "F", "S"];
const r = ["7", "-", "J", "S"];

function walk(pipes, start) {
  const connectionMap = new Map();
  connectionMap.set("|", [u, X, d, X]);
  connectionMap.set("-", [X, r, X, l]);
  connectionMap.set("L", [u, r, X, X]);
  connectionMap.set("J", [u, X, X, l]);
  connectionMap.set("7", [X, X, d, l]);
  connectionMap.set("F", [X, r, d, X]);
  connectionMap.set(".", [X, X, X, X]);
  connectionMap.set("S", [u, r, d, l]);

  const hashPosition = (row, col) => row * 10000 + col;
  const visited = new Set();
  let { row, col } = start;
  let steps = 0;
  do {
    const currentPipe = pipes[row][col];
    for (let d = UP; d <= LEFT; d++) {
      const newRow = row + directions[d][0];
      const newCol = col + directions[d][1];
      if (visited.has(hashPosition(newRow, newCol))) continue;
      const nextPipe = pipes[newRow] && pipes[newRow][newCol];
      if (connectionMap.get(currentPipe)[d].indexOf(nextPipe) >= 0) {
        row = newRow;
        col = newCol;
        visited.add(hashPosition(row, col));
        break;
      }
    }
    steps++;
  } while (pipes[row][col] != "S");
  return steps;
}

function problem1(pipes) {
  const steps = walk(pipes, findStart(pipes));
  return steps / 2;
}

const pipes = readLines(process.argv[2]);
console.log(problem1(pipes));
