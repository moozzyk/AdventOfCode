import { readFileSync } from "node:fs";
import { EOL } from "os";

function parse(line) {
  const [game, roundsText] = line.split(":");
  const [_, id] = game.split(" ");
  const rounds = [];
  for (let round of roundsText.split(";")) {
    const cube = { red: 0, green: 0, blue: 0 };
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
    if (round.red > red || round.green > green || round.blue > blue) {
      return false;
    }
  }
  return true;
}

function computeSetPower(game) {
  const { red, green, blue } = game.rounds.reduce(
    ({ red, green, blue }, round) => ({
      red: Math.max(red, round.red),
      green: Math.max(green, round.green),
      blue: Math.max(blue, round.blue),
    }),
    { red: 0, green: 0, blue: 0 }
  );

  return red * green * blue;
}

function problem1(games) {
  return games
    .map((g) =>
      isGamePossible(g, { red: 12, green: 13, blue: 14 }) ? g.id : 0
    )
    .reduce((res, n) => res + n, 0);
}

function problem2(games) {
  return games.map((g) => computeSetPower(g)).reduce((res, n) => res + n, 0);
}

const fileName = process.argv[2];
const games = readFileSync(fileName, "utf8")
  .split(EOL)
  .filter((l) => l.length > 0)
  .map((l) => parse(l));

console.log(problem1(games));
console.log(problem2(games));
