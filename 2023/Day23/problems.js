import { readLines } from "../utils.js";

function findPath({ startRow, startCol }, map) {
  let path = [{ r: startRow, c: startCol }];

  for (;;) {
    const { r, c } = path.at(-1);
    for (let { dr, dc } of [
      { dr: -1, dc: 0 },
      { dr: 1, dc: 0 },
      { dr: 0, dc: -1 },
      { dr: 0, dc: 1 },
    ]) {
      const newR = r + dr;
      const newC = c + dc;
      if (
        newR < 0 ||
        map[newR][newC] == "#" ||
        (path.length == 1 && (dc < 0 || dr < 0)) ||
        (path.length > 1 && newR == path.at(-2).r && newC == path.at(-2).c)
      ) {
        continue;
      }
      path.push({ r: newR, c: newC });

      if (map[newR][newC] == ".") {
        if (newR == map.length - 1) {
          return path;
        }
        break;
      }
      if (map[newR][newC] == ">") {
        path.push({ r: newR, c: newC + 1 });
        return path;
      }
      if (map[newR][newC] == "v") {
        path.push({ r: newR + 1, c: newC });
        return path;
      }
    }
  }
}

function longestPath(startRow, startCol, steps, map) {
  const path = findPath({ startRow, startCol }, map);
  steps += path.length;
  const { r, c } = path.at(-1);
  if (r == map.length - 1) {
    return steps - 1;
  }

  let maxSteps = 0;
  if (map[r + 1][c] == "v") {
    maxSteps = Math.max(maxSteps, longestPath(r + 1, c, steps, map));
  }
  if (map[r][c + 1] == ">") {
    maxSteps = Math.max(maxSteps, longestPath(r, c + 1, steps, map));
  }
  return maxSteps;
}

function problem1(map) {
  return longestPath(0, 1, 0, map);
}

const map = readLines(process.argv[2]);
console.log(problem1(map));
