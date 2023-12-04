import { readLines } from "../utils.js";

function parseCard(line) {
  const [id, numbers] = line.split(":");
  const [winning, chosen] = numbers.split("|");
  return {
    id: parseInt(id.substr(5)),
    winning: new Set(
      winning
        .split(" ")
        .map((n) => parseInt(n))
        .filter((n) => !isNaN(n))
    ),
    chosen: new Set(
      chosen
        .split(" ")
        .map((n) => parseInt(n))
        .filter((n) => !isNaN(n))
    ),
  };
}

function setIntersect(a, b) {
  const common = [];
  for (const [n] of a.entries()) {
    if (b.has(n)) {
      common.push(n);
    }
  }
  return common;
}

function problem1(lines) {
  const cards = lines.map((l) => parseCard(l));
  return cards
    .map((c) => setIntersect(c.winning, c.chosen))
    .map((n) => n.length)
    .filter((n) => n > 0)
    .map((n) => 2 ** (n - 1))
    .reduce((res, n) => res + n, 0);
}

function problem2(lines) {
  const cards = lines.map((l) => parseCard(l));
  const matchingNumbers = cards
    .map((c) => setIntersect(c.winning, c.chosen))
    .map((n) => n.length);

  const result = Array(matchingNumbers.length).fill(1);
  for (let i = 0; i < result.length; i++) {
    for (let j = 0; j < matchingNumbers[i]; j++) {
      result[i + j + 1] += result[i];
    }
  }
  return result.reduce((res, n) => res + n, 0);
}

const lines = readLines(process.argv[2]);

console.log(problem1(lines));
console.log(problem2(lines));
