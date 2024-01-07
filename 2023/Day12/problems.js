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

function memoizedCountMatches(start, groupIdx, groups, springs, cache) {
  const key = `${start}_${groupIdx}`;
  if (!cache.has(key)) {
    cache.set(key, countMatches(start, groupIdx, groups, springs, cache));
  }
  return cache.get(key);
}

function countMatches(start, groupIdx, groups, springs, cache) {
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
      matches += memoizedCountMatches(
        i + size + 1,
        groupIdx + 1,
        groups,
        springs,
        cache
      );
    }
    if (springs[i] == "#") {
      break;
    }
  }
  return matches;
}

function problem1(records) {
  const numWays = records.map(({ springs, groups }) =>
    countMatches(0, 0, groups, springs, new Map())
  );

  return sum(numWays);
}

function expandSprings({ springs, groups }) {
  return {
    springs: new Array(5).fill(springs).join("?"),
    groups: new Array(5).fill(groups).flat(),
  };
}

function problem2(records) {
  const numWays = records
    .map(expandSprings)
    .map(({ springs, groups }) =>
      countMatches(0, 0, groups, springs, new Map())
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
console.log(problem2(records));
