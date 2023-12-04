import { readLines } from "../utils.js";

function parseCard(line) {
  const [_, numbers] = line.split(":");
  const [winning, chosen] = numbers.split("|");
  return {
    winning: new Set(winning.trim(" ").split(/\s+/).map(Number)),
    chosen: new Set(chosen.trim(" ").split(/\s+/).map(Number)),
  };
}

function findMatchingNumbersCounts(cards) {
  return cards
    .map((c) => [...c.winning].filter((n) => c.chosen.has(n)))
    .map((n) => n.length);
}

function problem1(cards) {
  return findMatchingNumbersCounts(cards)
    .filter((n) => n > 0)
    .reduce((res, n) => res + 2 ** (n - 1), 0);
}

function problem2(cards) {
  const matchingNumbersCounts = findMatchingNumbersCounts(cards);
  const result = Array(matchingNumbersCounts.length).fill(1);
  for (let i = 0; i < result.length; i++) {
    for (let j = 0; j < matchingNumbersCounts[i]; j++) {
      result[i + j + 1] += result[i];
    }
  }
  return result.reduce((res, n) => res + n, 0);
}

const lines = readLines(process.argv[2]);
const cards = lines.map((l) => parseCard(l));

console.log(problem1(cards));
console.log(problem2(cards));
