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
