import { readLines, sum } from "../utils.js";

function isMatch(start, size, springs) {
  if (
    start + size > springs.length ||
    (start >= 0 && springs[start - 1] == "#") ||
    (start + size < springs.length && springs[start + size] == "#")
  ) {
    return false;
  }
  for (let i = 0; i < size; i++) {
    if (i == springs.length || springs[start + i] == ".") {
      return false;
    }
  }
  return true;
}

function countMatches(start, groupIdx, groups, springs) {
  if (groupIdx == groups.length) {
    for (let i = start; i < springs.length; i++) {
      if (springs[i] == "#") {
        return 0;
      }
    }
    return 1;
  }
  const size = groups[groupIdx];
  let matches = 0;
  for (let i = start; i < springs.length; i++) {
    if (isMatch(i, size, springs)) {
      matches += countMatches(i + size + 1, groupIdx + 1, groups, springs);
    }
    if (springs[i] == "#") {
      break;
    }
  }
  return matches;
}

function problem1(records) {
  const numWays = records.map(({ springs, groups }) =>
    countMatches(0, 0, groups, springs)
  );

  return sum(numWays);
}

const lines = readLines(process.argv[2]);
const records = lines
  .map((line) => line.split(" "))
  .map(([springs, groups]) => ({
    springs,
    groups: groups.split(",").map(Number),
  }));

console.log(problem1(records));
