import { open } from "fs/promises";

function getOuterDigits(s, digitMap) {
  let digits = [];
  for (let idx = 0; idx < s.length; idx++) {
    for (let digit of digitMap.keys()) {
      if (s.startsWith(digit, idx)) {
        digits.push([digit, idx]);
        continue;
      }
    }
  }

  digits.sort(([p, i], [p1, i1]) => i - i1);
  return [digitMap.get(digits.at(0)[0]), digitMap.get(digits.at(-1)[0])];
}

function solve(lines, digitMap) {
  return lines
    .map((line) =>
      getOuterDigits(line, digitMap).reduce((res, digit) => res * 10 + digit, 0)
    )
    .reduce((res, n) => res + n, 0);
}

function problem1(lines) {
  return solve(
    lines,
    new Map([
      ["1", 1],
      ["2", 2],
      ["3", 3],
      ["4", 4],
      ["5", 5],
      ["6", 6],
      ["7", 7],
      ["8", 8],
      ["9", 9],
    ])
  );
}

function problem2(lines) {
  return solve(
    lines,
    new Map([
      ["1", 1],
      ["2", 2],
      ["3", 3],
      ["4", 4],
      ["5", 5],
      ["6", 6],
      ["7", 7],
      ["8", 8],
      ["9", 9],
      ["one", 1],
      ["two", 2],
      ["three", 3],
      ["four", 4],
      ["five", 5],
      ["six", 6],
      ["seven", 7],
      ["eight", 8],
      ["nine", 9],
    ])
  );
}

async function run() {
  const fileName = process.argv[2];
  const file = await open(fileName);
  const lines = [];
  for await (const line of file.readLines()) {
    lines.push(line.toString());
  }
  console.log(problem1(lines));
  console.log(problem2(lines));
}

run();
