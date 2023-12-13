import { readFileSync } from "node:fs";
import { EOL } from "os";
import { sum } from "../utils.js";

function checkHorizontalSplit(mirror, row) {
  for (let i = 0; row - i >= 0 && row + i + 1 < mirror.length; i++) {
    if (mirror[row - i] != mirror[row + 1 + i]) {
      return false;
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

function findSplit(mirror) {
  for (let i = 0; i < mirror.length - 1; i++) {
    if (checkHorizontalSplit(mirror, i)) {
      return 100 * (i + 1);
    }
  }

  for (let i = 0; i < mirror[0].length - 1; i++) {
    if (checkVerticalSplit(mirror, i)) {
      return i + 1;
    }
  }
  throw new Error("Logic error");
}

function problem1(mirrors) {
  const result = mirrors.map(findSplit);
  return sum(result);
}

const mirrors = readFileSync(process.argv[2], "utf8")
  .split(`${EOL}${EOL}`)
  .map((l) => l.split(EOL).filter((l) => l.length > 0));

console.log(problem1(mirrors));
