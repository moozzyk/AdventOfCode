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

export function gcd(n1, n2) {
  if (n2 === 0) return n1;
  return gcd(n2, n1 % n2);
}

export function lcm(n) {
  return n.reduce((acc, n) => (acc * n) / gcd(acc, n), n[0]);
}

export function sum(a) {
  return a.reduce((acc, n) => acc + n, 0);
}

export function multiply(a) {
  return a.reduce((acc, n) => acc * n, 1);
}

export function rotateRightInPlace(a) {
  if (a.length != a[0].length) {
    throw new Error("The array needs to be rectangular");
  }

  for (let i = 0; i < a.length / 2; i++) {
    for (let j = 0; j < a.length - 2 * i - 1; j++) {
      const topRow = i;
      const bottomRow = a.length - i - 1;
      const leftCol = i;
      const rightCol = a.length - i - 1;

      const tmp = a[topRow][leftCol + j];
      a[topRow][leftCol + j] = a[bottomRow - j][leftCol];
      a[bottomRow - j][leftCol] = a[bottomRow][rightCol - j];
      a[bottomRow][rightCol - j] = a[topRow + j][rightCol];
      a[topRow + j][rightCol] = tmp;
    }
  }
}
