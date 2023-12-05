import { readLines, isDigit } from "../utils.js";

function parse(lines) {
  let seeds = lines[0].split(" ").slice(1).map(Number);
  const almanac = [];
  const buffer = [];
  for (const line of lines.slice(2)) {
    if (!isDigit(line[0])) {
      almanac.push([...buffer]);
      buffer.length = 0;
    } else {
      buffer.push(line.split(" ").map(Number));
    }
  }
  almanac.push([...buffer]);
  return { seeds, almanac };
}

function findLocation(seed, almanac) {
  for (let i = 0; i < almanac.length; i++) {
    for (let [dest, src, length] of almanac[i]) {
      if (seed >= src && seed < src + length) {
        seed = dest + (seed - src);
        break;
      }
    }
  }
  return seed;
}

function problem1(lines) {
  const { seeds, almanac } = parse(lines);
  return seeds
    .map((s) => findLocation(s, almanac))
    .reduce((res, n) => Math.min(res, n), 100000000000000);
}

const lines = readLines(process.argv[2]);
console.log(problem1(lines));
