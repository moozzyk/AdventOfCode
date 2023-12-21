import { readLines } from "../utils.js";

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

function findNumSteps({ startRow, startCol }, { row, col }, garden) {
  const visited = new Set();
  const q = [{ r: row, c: col, s: 0 }];
  let qIdx = 0;
  while (qIdx < q.length) {
    const { r, c, s } = q[qIdx++];
    if (r == startRow && c == startCol) {
      return s;
    }
    const hash = `${r}_${c}`;
    if (visited.has(hash)) continue;
    visited.add(hash);
    if (r < garden.length - 1 && garden[r + 1][c] != "#") {
      q.push({ r: r + 1, c, s: s + 1 });
    }
    if (r > 0 && garden[r - 1][c] != "#") {
      q.push({ r: r - 1, c, s: s + 1 });
    }
    if (c < garden[0].length - 1 && garden[r][c + 1] != "#") {
      q.push({ r, c: c + 1, s: s + 1 });
    }
    if (c > 0 && garden[r][c - 1] != "#") {
      q.push({ r, c: c - 1, s: s + 1 });
    }
  }
  return Number.MAX_SAFE_INTEGER;
}

function problem1(garden1, numSteps) {
  const garden = garden1.map((r) => r.split(""));
  const { r: startRow, c: startCol } = findStart(garden);
  let numFields = 0;
  for (let row = 0; row < garden.length; row++) {
    for (
      let col = (startCol % 2) + ((row % 2) - 1);
      col < garden[0].length;
      col += 2
    ) {
      if (
        garden[row][col] != "#" &&
        Math.abs(startRow - row) + Math.abs(startCol - col) <= numSteps
      ) {
        if (
          findNumSteps({ startRow, startCol }, { row, col }, garden) <= numSteps
        ) {
          garden[row][col] = "O";
          numFields++;
        }
      }
    }
  }
  garden.map((r) => r.join("")).forEach((l) => console.log(l));
  return numFields;
}
const garden = readLines(process.argv[2]);
console.log(problem1(garden, Number(process.argv[3])));
