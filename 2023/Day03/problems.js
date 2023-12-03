import { readFileSync } from "node:fs";
import { EOL } from "os";

function isDigit(c) {
  return c >= "0" && c <= "9";
}

function isPartSymbol(lines, row, col) {
  return !(
    row < 0 ||
    row >= lines.length ||
    col < 0 ||
    col >= lines[row].length ||
    isDigit(lines[row][col]) ||
    lines[row][col] === "."
  );
}

function maybeGetPartNumber(lines, row, col) {
  let partNumber = "";
  let isPart = false;
  while (col < lines[row].length && isDigit(lines[row][col])) {
    partNumber += lines[row][col];
    for (let r = row - 1; r <= row + 1; r++) {
      for (let c = col - 1; c <= col + 1; c++) {
        isPart = isPart || isPartSymbol(lines, r, c);
      }
    }
    col++;
  }
  return { partNumber, isPart, newColumn: col };
}

function findPartNumbers(lines) {
  const parts = [];
  for (let row = 0; row < lines.length; row++) {
    let col = 0;
    while (col < lines[row].length) {
      const c = lines[row][col];
      if (isDigit(c)) {
        const { partNumber, isPart, newColumn } = maybeGetPartNumber(
          lines,
          row,
          col
        );
        if (isPart) {
          parts.push(parseInt(partNumber));
        }
        col = newColumn;
      } else {
        col++;
      }
    }
  }
  return parts;
}

function problem1(lines) {
  const parts = findPartNumbers(lines);
  return parts.reduce((res, n) => res + n, 0);
}

const fileName = process.argv[2];
const lines = readFileSync(fileName, "utf8")
  .split(EOL)
  .filter((l) => l.length > 0);

console.log(problem1(lines));
