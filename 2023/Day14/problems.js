import { sum, readLines } from "../utils.js";

function computeNorthLoad(platform) {
  let load = 0;
  for (let col = 0; col < platform[0].length; col++) {
    for (let row = 0; row < platform.length; row++) {
      if (platform[row][col] == "O") {
        load += platform.length - row;
      }
    }
  }
  return load;
}

function tiltNorth(platform) {
  for (let col = 0; col < platform.length; col++) {
    let targetRow = 0;
    for (let row = 0; row < platform.length; row++) {
      if (platform[row][col] == "#") {
        targetRow = row + 1;
      }
      if (platform[row][col] == "O") {
        if (row != targetRow) {
          platform[targetRow][col] = "O";
          platform[row][col] = ".";
        }
        targetRow++;
      }
    }
  }
}

function tiltWest(platform) {
  for (let row = 0; row < platform.length; row++) {
    let targetCol = 0;
    for (let col = 0; col < platform.length; col++) {
      if (platform[row][col] == "#") {
        targetCol = col + 1;
      }
      if (platform[row][col] == "O") {
        if (col != targetCol) {
          platform[row][targetCol] = "O";
          platform[row][col] = ".";
        }
        targetCol++;
      }
    }
  }
}

function tiltSouth(platform) {
  for (let col = 0; col < platform.length; col++) {
    let targetRow = platform.length - 1;
    for (let row = platform.length - 1; row >= 0; row--) {
      if (platform[row][col] == "#") {
        targetRow = row - 1;
      }
      if (platform[row][col] == "O") {
        if (row != targetRow) {
          platform[targetRow][col] = "O";
          platform[row][col] = ".";
        }
        targetRow--;
      }
    }
  }
}

function tiltEast(platform) {
  for (let row = 0; row < platform.length; row++) {
    let targetCol = platform.length - 1;
    for (let col = platform.length - 1; col >= 0; col--) {
      if (platform[row][col] == "#") {
        targetCol = col - 1;
      }
      if (platform[row][col] == "O") {
        if (col != targetCol) {
          platform[row][targetCol] = "O";
          platform[row][col] = ".";
        }
        targetCol--;
      }
    }
  }
}

function cycle(platform) {
  tiltNorth(platform);
  tiltWest(platform);
  tiltSouth(platform);
  tiltEast(platform);
}

function problem1(platform) {
  let p = platform.map((l) => l.split(""));
  tiltNorth(p);
  return computeNorthLoad(p);
}

function problem2(platform) {
  let p = platform.map((l) => l.split(""));
  const tracker = new Map();
  for (let i = 0; i < 1000000000; i++) {
    cycle(p);
    const key = p.map((l) => l.join("")).join("");
    if (tracker.has(key)) {
      const cycleStart = tracker.get(key);
      const cycleLength = i - cycleStart;
      const endCycles = (1000000000 - cycleStart) % cycleLength;
      i = 1000000000 - endCycles;
      tracker.clear();
    }
    tracker.set(key, i);
  }
  return computeNorthLoad(p.map((l) => l.join("")));
}

const platform = readLines(process.argv[2]);
console.log(problem1(platform));
console.log(problem2(platform));
