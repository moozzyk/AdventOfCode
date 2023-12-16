import { readLines, sum } from "../utils.js";

const UP = 0;
const RIGHT = 1;
const DOWN = 2;
const LEFT = 3;

function run(contraption, start) {
  const beams = Array.from({ length: contraption.length }, () =>
    Array.from({ length: contraption[0].length }, () => [
      false,
      false,
      false,
      false,
    ])
  );
  const q = [start];
  while (q.length > 0) {
    let { direction, row, col } = q.shift();
    while (
      row >= 0 &&
      row < contraption.length &&
      col >= 0 &&
      col < contraption[0].length
    ) {
      if (beams[row][col][direction]) break;
      beams[row][col][direction] = true;
      if (contraption[row][col] == ".") {
      } else if (contraption[row][col] == "/") {
        if (direction == RIGHT) direction = UP;
        else if (direction == UP) direction = RIGHT;
        else if (direction == LEFT) direction = DOWN;
        else if (direction == DOWN) direction = LEFT;
      } else if (contraption[row][col] == "\\") {
        if (direction == RIGHT) direction = DOWN;
        else if (direction == DOWN) direction = RIGHT;
        else if (direction == LEFT) direction = UP;
        else if (direction == UP) direction = LEFT;
      } else if (contraption[row][col] == "-") {
        if (direction == UP || direction == DOWN) {
          q.push({ direction: LEFT, row: row, col: col - 1 });
          q.push({ direction: RIGHT, row: row, col: col + 1 });
          break;
        }
      } else if (contraption[row][col] == "|") {
        if (direction == LEFT || direction == RIGHT) {
          q.push({ direction: UP, row: row - 1, col: col });
          q.push({ direction: DOWN, row: row + 1, col: col });
          break;
        }
      } else {
        throw new Error("logic error");
      }
      if (direction == UP) row--;
      if (direction == RIGHT) col++;
      if (direction == DOWN) row++;
      if (direction == LEFT) col--;
    }
  }

  return sum(
    beams.map((b) =>
      b.reduce((acc, t) => acc + (t.indexOf(true) >= 0 ? 1 : 0), 0)
    )
  );
}

function problem1(contraption) {
  return run(contraption, { direction: RIGHT, row: 0, col: 0 });
}

function problem2(contraption) {
  const res = [];
  for (let row = 0; row < contraption.length; row++) {
    res.push(run(contraption, { direction: RIGHT, row, col: 0 }));
    res.push(
      run(contraption, { direction: LEFT, row, col: contraption[0].length - 1 })
    );
  }
  for (let col = 0; col < contraption[0].length; col++) {
    res.push(run(contraption, { direction: DOWN, row: 0, col }));
    res.push(
      run(contraption, { direction: UP, row: contraption.length - 1, col })
    );
  }
  return Math.max(...res);
}

const lines = readLines(process.argv[2]);
console.log(problem1(lines));
console.log(problem2(lines));
