import { readFileSync } from "node:fs";
import { EOL } from "os";

function parse(line) {
  const [game, roundsText] = line.split(":");
  const [_, id] = game.split(" ");
  const cube = { red: 0, green: 0, blue: 0 };
  for (let round of roundsText.split(";")) {
    for (let c of round.split(",")) {
      const [count, color] = c.trim().split(" ");
      cube[color] = Math.max(cube[color], parseInt(count));
    }
  }
  return { id: parseInt(id), cube };
}

function problem1(games) {
  return games
    .map(({ id, cube }) =>
      cube.red <= 12 && cube.green <= 13 && cube.blue <= 14 ? id : 0
    )
    .reduce((res, n) => res + n, 0);
}

function problem2(games) {
  return games
    .map(({ cube: { red, green, blue } }) => red * green * blue)
    .reduce((res, n) => res + n, 0);
}

const fileName = process.argv[2];
const games = readFileSync(fileName, "utf8")
  .split(EOL)
  .filter((l) => l.length > 0)
  .map((l) => parse(l));

console.log(problem1(games));
console.log(problem2(games));
