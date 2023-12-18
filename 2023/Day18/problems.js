import { readLines } from "../utils.js";

const deltas = new Map([
  ["U", [-1, 0]],
  ["D", [1, 0]],
  ["R", [0, 1]],
  ["L", [0, -1]],
]);

function parse(lines) {
  const regex = /(\w) (\d+).+#(.+)\)/g;
  return lines
    .map((l) => [...l.matchAll(regex)])
    .map(([[_, dir, steps, color]]) => ({
      dir,
      steps,
      color,
    }));
}

function fill(lagoon) {
  let q = [{ row: 0, col: 0 }];
  while (q.length > 0) {
    const { row, col } = q.shift();
    if (
      !lagoon[row] ||
      !lagoon[row][col] ||
      lagoon[row][col] == "o" ||
      lagoon[row][col] == "#"
    )
      continue;
    lagoon[row][col] = "o";
    q.push({ row: row - 1, col });
    q.push({ row: row + 1, col });
    q.push({ row, col: col - 1 });
    q.push({ row, col: col + 1 });
  }
}

function problem1(plan) {
  const length = 1000;
  const lagoon = Array.from({ length }, () => Array(length).fill("."));
  let row = length / 2,
    col = length / 2;
  lagoon[row][col] = "#";
  for (const { dir, steps, color } of plan) {
    const [rowDelta, colDelta] = deltas.get(dir);
    for (let i = 0; i < steps; i++) {
      row += rowDelta;
      col += colDelta;
      lagoon[row][col] = "#";
    }
  }
  fill(lagoon, 0, 0);
  let lagoonSize = 0;
  for (let r = 0; r < lagoon.length; r++) {
    for (let c = 0; c < lagoon[r].length; c++) {
      if (lagoon[r][c] != "o") {
        lagoonSize++;
      }
    }
  }
  return lagoonSize;
}

const lines = readLines(process.argv[2]);
const plan = parse(lines);
console.log(problem1(plan));
