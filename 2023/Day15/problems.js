import { readLines, sum } from "../utils.js";

function computeHash(s) {
  return s
    .split("")
    .reduce((acc, s) => (17 * (acc + s.charCodeAt(0))) % 256, 0);
}

function problem1(steps) {
  return sum(steps.map(computeHash));
}

function findLens(box, label) {
  for (let i = 0; i < box.length; i++) {
    if (box[i][0] == label) {
      return i;
    }
  }
  return -1;
}

function addLens(box, label, lens) {
  const lensIdx = findLens(box, label);
  if (lensIdx >= 0) {
    box[lensIdx][1] = lens;
  } else {
    box.push([label, lens]);
  }
}

function removeLens(box, label) {
  const lensIdx = findLens(box, label);
  if (lensIdx >= 0) {
    box.splice(lensIdx, 1);
  }
}

function computePower(box, boxId) {
  return box.map(([_, lens], i) => (1 + boxId) * (i + 1) * Number(lens));
}

function problem2(steps) {
  const regex = /(\w+)(.)(\d?)/g;
  const boxes = Array.from({ length: 256 }, () => []);
  for (const s of steps) {
    const [[_, label, op, lens]] = [...s.matchAll(regex)];
    const box = boxes[computeHash(label)];
    if (op == "=") {
      addLens(box, label, lens);
    } else {
      removeLens(box, label);
    }
  }

  const power = boxes.map((box, idx) => computePower(box, idx));
  return sum(power.flat());
}

const steps = readLines(process.argv[2])[0].split(",");
console.log(problem1(steps));
console.log(problem2(steps));
