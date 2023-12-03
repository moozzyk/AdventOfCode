import { readFileSync } from "node:fs";
import { EOL } from "os";

function isDigit(c) {
  return c && c >= "0" && c <= "9";
}

function getPartAt(lines, row, col) {
  if (row < 0 || row >= lines.length || !isDigit(lines[row][col])) {
    return "";
  }
  let part = "";
  while (col > 0 && isDigit(lines[row][col - 1])) {
    col--;
  }
  while (col < lines[row].length && isDigit(lines[row][col])) {
    part += lines[row][col];
    col++;
  }
  return part;
}

function findPartsAt(lines, row, col) {
  const parts = [];
  parts.push(getPartAt(lines, row - 1, col));
  parts.push(getPartAt(lines, row + 1, col));
  parts.push(getPartAt(lines, row, col - 1));
  parts.push(getPartAt(lines, row, col + 1));
  if (!isDigit(lines[row - 1][col])) {
    parts.push(getPartAt(lines, row - 1, col - 1));
    parts.push(getPartAt(lines, row - 1, col + 1));
  }
  if (!isDigit(lines[row + 1][col])) {
    parts.push(getPartAt(lines, row + 1, col - 1));
    parts.push(getPartAt(lines, row + 1, col + 1));
  }
  return parts.filter((g) => g.length > 0).map((g) => parseInt(g));
}

function findParts(lines) {
  const parts = [];
  for (let row = 0; row < lines.length; row++) {
    for (let col = 0; col < lines[row].length; col++) {
      if (!isDigit(lines[row][col]) && lines[row][col] !== ".") {
        parts.push(...findPartsAt(lines, row, col));
      }
    }
  }
  return parts;
}

function problem1(lines) {
  const parts = findParts(lines);
  return parts.reduce((res, n) => res + n, 0);
}

function findGears(lines) {
  const gears = [];
  for (let row = 0; row < lines.length; row++) {
    for (let col = 0; col < lines[row].length; col++) {
      if (lines[row][col] === "*") {
        const parts = findPartsAt(lines, row, col);
        if (parts.length == 2) {
          gears.push(parts[0] * parts[1]);
        }
      }
    }
  }
  return gears;
}

function problem2(lines) {
  const gears = findGears(lines);
  return gears.reduce((res, n) => res + n, 0);
}

const fileName = process.argv[2];
const lines = readFileSync(fileName, "utf8")
  .split(EOL)
  .filter((l) => l.length > 0);

console.log(problem1(lines));
console.log(problem2(lines));
