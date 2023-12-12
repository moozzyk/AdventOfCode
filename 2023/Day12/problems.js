import { readLines } from "../utils.js";

function sum(a) {
  return a.reduce((acc, n) => acc + n, 0);
}
function isMatch(springs, candidate) {
  for (let i = 0; i < candidate.length; i++) {
    if (springs[i] != "?" && springs[i] != candidate[i]) {
      return false;
    }
  }
  return true;
}

function countNumWays(springs, arrangment, candidate) {
  if (
    candidate.length + sum(arrangment) + arrangment.length - 1 >
      springs.length ||
    !isMatch(springs, candidate)
  ) {
    return 0;
  }
  if (candidate.length == springs.length) {
    return 1;
  }

  let result = 0;
  if (
    arrangment.length > 0 &&
    (candidate.length == 0 || candidate.at(-1) == ".")
  ) {
    result = countNumWays(
      springs,
      arrangment.slice(1),
      candidate + "#".repeat(arrangment[0])
    );
  }

  return result + countNumWays(springs, arrangment, candidate + ".");
}

function problem1(records) {
  const numWays = records.map(({ springs, arrangment }) =>
    countNumWays(springs, arrangment, "")
  );

  return sum(numWays);
}

const lines = readLines(process.argv[2]);
const records = lines
  .map((line) => line.split(" "))
  .map(([springs, arrangment]) => ({
    springs,
    arrangment: arrangment.split(",").map(Number),
  }));
console.log(problem1(records));
