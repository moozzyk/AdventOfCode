import { readLines } from "../utils.js";

const UP = 0;
const RIGHT = 1;
const DOWN = 2;
const LEFT = 3;

const rowDelta = [-1, 0, 1, 0];
const colDelta = [0, 1, 0, -1];

const spliceLimit = 50000;
function walk(map, minStepsRequired, maxSteps) {
  const visited = new Map();
  let q = [
    {
      row: 0,
      col: 0,
      dir: RIGHT,
      stepsRequired: minStepsRequired,
      stepsLeft: maxSteps + 1,
      heat: 0,
      path: [],
    },
  ];
  let minHeat = Number.MAX_SAFE_INTEGER;
  let head = 0;
  while (q.length > 0 && head < q.length) {
    const { row, col, dir, stepsRequired, stepsLeft, heat, path } = q[head++];
    if (head == spliceLimit) {
      q.splice(0, spliceLimit);
      head = 0;
    }
    const newHeat = heat + Number(map[row][col]);
    if (row == map.length - 1 && col == map[0].length - 1) {
      if (minHeat > newHeat - Number(map[0][0])) {
        minHeat = Math.min(minHeat, newHeat - Number(map[0][0]));

        if (!path) continue;
        console.log(minHeat);
        console.log(path);
        const tmp = [];
        for (let r = 0; r < map.length; r++) {
          tmp.push([]);
          for (let c = 0; c < map[0].length; c++) {
            tmp[r][c] = map[r][c];
          }
        }
        let c = 0,
          r = 0;
        let heatVerify = 0;
        for (let d of path) {
          heatVerify += Number(map[r][c]);
          if (d == RIGHT) tmp[r][c] = ">";
          if (d == LEFT) tmp[r][c] = "<";
          if (d == UP) tmp[r][c] = "^";
          if (d == DOWN) tmp[r][c] = "v";
          r += rowDelta[d];
          c += colDelta[d];
        }
        tmp.map((l) => l.join("")).forEach((l) => console.log(l));
        console.log(heatVerify);
        console.log("---");
      }
      continue;
    }
    const key = `${row}_${col}_${dir}_${stepsRequired}_${stepsLeft}`;
    if (visited.has(key) && visited.get(key) <= heat) {
      continue;
    }
    visited.set(key, heat);
    if (newHeat > minHeat) {
      continue;
    }

    const availDir = [];
    if (stepsLeft > 0) {
      availDir.push(dir);
    }
    if (stepsRequired == 0) {
      availDir.push((dir + 4 - 1) % 4);
      availDir.push((dir + 4 + 1) % 4);
    }

    for (const newDir of availDir) {
      const newRow = row + rowDelta[newDir];
      const newCol = col + colDelta[newDir];
      const newStepsLeft = dir == newDir ? stepsLeft - 1 : maxSteps;
      const newStepsRequired =
        dir == newDir ? Math.max(stepsRequired - 1, 0) : minStepsRequired - 1;
      if (
        newRow < 0 ||
        newCol < 0 ||
        newRow == map.length ||
        newCol == map[0].length ||
        newStepsLeft == 0
      ) {
        continue;
      }

      q.push({
        row: newRow,
        col: newCol,
        dir: newDir,
        stepsRequired: newStepsRequired,
        stepsLeft: newStepsLeft,
        heat: newHeat,
        // path: [...path, newDir], // debug
      });
    }
  }
  return minHeat;
}

function problem1(map) {
  return walk(map, 1, 3);
}

function problem2(map) {
  return walk(map, 4, 10);
}

const map = readLines(process.argv[2]);
console.log(problem1(map));
console.log(problem2(map));
