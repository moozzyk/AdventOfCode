import { sum, readLines } from "../utils.js";

function computeColumnLoad(platform, col) {
  let load = 0;
  let nextRowLoad = platform.length;
  for (let row = 0; row < platform.length; row++) {
    if (platform[row][col] == "#") {
      nextRowLoad = platform.length - row - 1;
    }
    if (platform[row][col] == "O") {
      load += nextRowLoad;
      nextRowLoad--;
    }
  }
  return load;
}
function problem1(platform) {
  return sum(
    platform[0].split("").map((item, col) => computeColumnLoad(platform, col))
  );
}

const platform = readLines(process.argv[2]);
console.log(problem1(platform));
