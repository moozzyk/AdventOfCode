import { readFileSync } from "node:fs";
import { EOL } from "os";

export function isDigit(c) {
  return c && c >= "0" && c <= "9";
}

export function readLines(fileName) {
  return readFileSync(fileName, "utf8")
    .split(EOL)
    .filter((l) => l.length > 0);
}
