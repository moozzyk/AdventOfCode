import { readFileSync } from "node:fs";
import { EOL } from "os";
import { sum } from "../utils.js";

function checkHorizontalSplit(mirror, row) {
  for (let i = 0; row - i >= 0 && row + i + 1 < mirror.length; i++) {
    for (let j = 0; j < mirror[0].length; j++) {
      if (mirror[row - i][j] != mirror[row + 1 + i][j]) {
        return false;
      }
    }
  }
  return true;
}

function checkVerticalSplit(mirror, col) {
  for (let i = 0; col - i >= 0 && col + 1 + i < mirror[0].length; i++) {
    for (let j = 0; j < mirror.length; j++) {
      if (mirror[j][col - i] != mirror[j][col + 1 + i]) {
        return false;
      }
    }
  }
  return true;
}

function findSplit(mirror, skipSplit) {
  for (let i = 0; i < mirror.length - 1; i++) {
    if (checkHorizontalSplit(mirror, i)) {
      const score = 100 * (i + 1);
      if (score != skipSplit) return score;
    }
  }

  for (let i = 0; i < mirror[0].length - 1; i++) {
    if (checkVerticalSplit(mirror, i)) {
      const score = i + 1;
      if (score != skipSplit) {
        return i + 1;
      }
    }
  }
  return undefined;
}

function problem1(mirrors) {
  const result = mirrors.map(findSplit);
  return sum(result);
}

function findSmudges(mirror) {
  const originalSplit = findSplit(mirror);
  for (let row = 0; row < mirror.length; row++) {
    for (let col = 0; col < mirror[row].length; col++) {
      const tmp = mirror[row][col];
      mirror[row][col] = tmp == "#" ? "." : "#";
      const split = findSplit(mirror, originalSplit);
      if (split) {
        return split;
      }
      mirror[row][col] = tmp;
    }
  }
  throw new Error("Logic error");
}

function problem2(mirrors) {
  const result = mirrors.map(findSmudges);
  return sum(result);
}

const mirrors = readFileSync(process.argv[2], "utf8")
  .split(`${EOL}${EOL}`)
  .map((l) =>
    l
      .split(EOL)
      .filter((l) => l.length > 0)
      .map((l) => l.split(""))
  );
// console.log(mirrors);
console.log(problem1(mirrors));
console.log(problem2(mirrors));
