import { readFileSync } from "node:fs";
import { EOL } from "os";

function parse(line) {
  const [game, roundsText] = line.split(":");
  const [_, id] = game.split(" ");
  const rounds = [];
  for (let round of roundsText.split(";")) {
    const cube = {};
    for (let c of round.split(",")) {
      const [count, color] = c.trim().split(" ");
      cube[color] = parseInt(count);
    }
    rounds.push(cube);
  }
  return { id: parseInt(id), rounds };
}

function isGamePossible(game, { red, green, blue }) {
  for (let round of game.rounds) {
    if (
      (round.red ?? 0) > red ||
      (round.green ?? 0) > green ||
      (round.blue ?? 0) > blue
    ) {
      return false;
    }
  }
  return true;
}

function problem1(lines) {
  return lines
    .map((l) => parse(l))
    .map((g) =>
      isGamePossible(g, { red: 12, green: 13, blue: 14 }) ? g.id : 0
    )
    .reduce((res, n) => res + n, 0);
}

const fileName = process.argv[2];
const lines = readFileSync(fileName, "utf8")
  .split(EOL)
  .filter((l) => l.length > 0);

console.log(problem1(lines));
