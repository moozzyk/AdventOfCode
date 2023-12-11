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

const hashPosition = (row, col) => row * 10000 + col;

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

  const path = [];
  let { row, col } = start;
  let steps = [];
  do {
    const currentPipe = pipes[row][col];
    for (let d = UP; d <= LEFT; d++) {
      const newRow = row + directions[d][0];
      const newCol = col + directions[d][1];
      const nextPipe = pipes[newRow] && pipes[newRow][newCol];
      if (!nextPipe) continue;
      if (
        path.length > 0 &&
        path.at(-1).row == newRow &&
        path.at(-1).col == newCol
      )
        continue;
      if (connectionMap.get(currentPipe)[d].indexOf(nextPipe) >= 0) {
        path.push({ row, col });
        row = newRow;
        col = newCol;
        break;
      }
    }
    steps++;
    // A hacky way to detect the entire loop cannot be walked. Use in the
    // second problem when testing if S replacement is valid and closes the pipe.
    if (steps > pipes.length * pipes[0].length) return path;
  } while (!(row == start.row && col == start.col));
  return path;
}

function problem1(path) {
  return path.length / 2;
}

function replaceS(pipes, path) {
  for (const newS of "F-|7FLJ") {
    const newPipes = pipes.map((s) => s.replaceAll("S", newS));
    const newPath = walk(newPipes, path[0]);
    // If the S replacement is invalid we can't walk the entire path
    // either we would walk only first element or we can't take the last step.
    if (newPath.length == path.length) {
      return newPipes;
    }
  }
  throw new Error("Can't replace S");
}

function problem2(pipesOrig, path) {
  const pipes = replaceS(pipesOrig, path);
  const visited = new Set(path.map(({ row, col }) => hashPosition(row, col)));
  let insideCount = 0;
  for (let row = 0; row < pipes.length; row++) {
    let outside = true;
    let lastTurn = "";

    for (let col = 0; col < pipes[row].length; col++) {
      const posHash = hashPosition(row, col);
      const currentPipe = pipes[row][col];
      if (!visited.has(posHash)) {
        if (!outside) {
          insideCount++;
        }
        continue;
      }

      if (currentPipe == "-") continue;
      if (currentPipe == "|") {
        lastTurn = "";
        outside = !outside;
        continue;
      }
      if (lastTurn == "") {
        lastTurn = currentPipe;
        continue;
      }

      switch (`${lastTurn}${currentPipe}`) {
        case "FJ":
        case "JF":
        case "L7":
        case "7L":
          outside = !outside;
          break;
      }
      lastTurn = "";
    }
  }

  return insideCount;
}

const pipes = readLines(process.argv[2]);
const path = walk(pipes, findStart(pipes));
console.log(problem1(path));
console.log(problem2(pipes, path));
