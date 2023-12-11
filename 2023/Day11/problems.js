import { readLines, isDigit } from "../utils.js";

function countSteps(from, to, duplicates, expansionFactor) {
  let distance = 0;
  for (let i = Math.min(from, to); i < Math.max(from, to); i++) {
    distance++;
    if (duplicates.has(i)) {
      distance += expansionFactor;
    }
  }
  return distance;
}

function solve(image, expansionFactor) {
  const galaxies = [];
  for (let row = 0; row < image.length; row++) {
    for (let col = 0; col < image[row].length; col++) {
      if (image[row][col] == "#") {
        galaxies.push({ row, col });
      }
    }
  }
  const duplicateRows = new Set(
    Array(image.length)
      .fill()
      .map((_, i) => i)
  );

  const duplicateCols = new Set(
    Array(image[0].length)
      .fill()
      .map((_, i) => i)
  );

  galaxies.forEach(({ row, col }) => {
    duplicateRows.delete(row);
    duplicateCols.delete(col);
  });

  const distances = [];
  for (let from = 0; from < galaxies.length; from++) {
    for (let to = from + 1; to < galaxies.length; to++) {
      let distance =
        countSteps(
          galaxies[from].row,
          galaxies[to].row,
          duplicateRows,
          expansionFactor
        ) +
        countSteps(
          galaxies[from].col,
          galaxies[to].col,
          duplicateCols,
          expansionFactor
        );
      distances.push(distance);
    }
  }
  return distances.reduce((acc, n) => acc + n, 0);
}

function problem1(image) {
  return solve(image, 1);
}

function problem2(image) {
  return solve(image, 999999);
}

const image = readLines(process.argv[2]);
console.log(problem1(image));
console.log(problem2(image));
