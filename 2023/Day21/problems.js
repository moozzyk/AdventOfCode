import { readLines, mod } from "../utils.js";

function findStart(garden) {
  for (let r = 0; r < garden.length; r++) {
    for (let c = 0; c < garden[0].length; c++) {
      if (garden[r][c] == "S") {
        return { r, c };
      }
    }
  }
  throw new Error("Couldn't find starting pont.");
}

function getTile(garden, row, col, startRow, startCol) {
  const newRow = mod(startRow - row, garden.length);
  const newCol = mod(startCol + col, garden[0].length);
  return garden[newRow][newCol];
}

function visitGarden(garden, maxSteps) {
  const { r: startRow, c: startCol } = findStart(garden);
  const visited = new Map();
  const q = [{ r: 0, c: 0, s: 0 }];
  let qIdx = 0;
  while (qIdx < q.length) {
    const { r, c, s } = q[qIdx++];
    const posHash = `${r}_${c}`;
    if (visited.has(posHash) || s > maxSteps) {
      continue;
    }
    visited.set(posHash, s);
    if (getTile(garden, r + 1, c, startRow, startCol) != "#") {
      q.push({ r: r + 1, c, s: s + 1 });
    }
    if (getTile(garden, r - 1, c, startRow, startCol) != "#") {
      q.push({ r: r - 1, c: c, s: s + 1 });
    }
    if (getTile(garden, r, c + 1, startRow, startCol) != "#") {
      q.push({ r, c: c + 1, s: s + 1 });
    }
    if (getTile(garden, r, c - 1, startRow, startCol) != "#") {
      q.push({ r, c: c - 1, s: s + 1 });
    }
  }
  return visited;
}

function problem1(garden, maxSteps) {
  const visited = visitGarden(garden, maxSteps);
  return [...visited.values()].filter((v) => v % 2 == maxSteps % 2).length;
}

const garden = readLines(process.argv[2]).map((r) => r.split(""));
const maxSteps = Number(process.argv[3]);
console.log(problem1(garden, maxSteps));
