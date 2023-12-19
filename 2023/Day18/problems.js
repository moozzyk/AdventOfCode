import { readLines } from "../utils.js";

const deltas = new Map([
  ["R", [0, 1]],
  ["0", [0, 1]],
  ["D", [1, 0]],
  ["1", [1, 0]],
  ["L", [0, -1]],
  ["2", [0, -1]],
  ["U", [-1, 0]],
  ["3", [-1, 0]],
]);

function parse(lines) {
  const regex = /(\w) (\d+).+#(.+)\)/g;
  return lines
    .map((l) => [...l.matchAll(regex)])
    .map(([[_, dir, steps, color]]) => ({
      dir,
      steps,
      color,
    }));
}

const CROSS_EDGE = 1;
const OVER_EDGE = 2;
const UNDER_EDGE = 3;

function getEdgeType(edge, row) {
  const [top, , bottom] = edge;
  if (top < row && bottom > row) return CROSS_EDGE;
  if (top < row && bottom == row) return OVER_EDGE;
  if (top == row && bottom > row) return UNDER_EDGE;
  throw new Error("Unexpected edge");
}

function getSweepRows(edges) {
  return [...new Set(edges.map((e) => [e[0], e[2]]).flat()).values()].toSorted(
    (a, b) => a - b
  );
}

function findVolume(edges) {
  edges.sort((e1, e2) => e1[1] - e2[1]);
  const sweepRows = getSweepRows(edges);
  let volume = 0;
  // compute volume not including sweep rows
  for (let i = 0; i < sweepRows.length - 1; i++) {
    const topRow = sweepRows[i];
    const bottomRow = sweepRows[i + 1];
    if (bottomRow - topRow == 1) continue;
    const crossEdges = edges.filter(
      (e) => e[0] < topRow + 1 && e[2] > topRow + 1
    );
    for (let j = 0; j < crossEdges.length; j += 2) {
      volume +=
        (bottomRow - topRow - 1) *
        (crossEdges[j + 1][1] - crossEdges[j][1] + 1);
    }
  }
  // compute volume of only sweep rows
  for (const row of sweepRows) {
    const verticalEdges = edges.filter(
      (e) => e[0] != e[2] && e[0] <= row && e[2] >= row
    );
    let inside = false;
    let openEdge = verticalEdges[0];
    for (let j = 0; j < verticalEdges.length; j++) {
      const edgeType = getEdgeType(verticalEdges[j], row);
      if (edgeType == CROSS_EDGE) {
        inside = !inside;
      } else {
        j++;
        if (edgeType != getEdgeType(verticalEdges[j], row)) {
          inside = !inside;
        }
      }
      if (!inside) {
        volume += 1 + verticalEdges[j][3] - openEdge[1];
        openEdge = verticalEdges[j + 1];
      }
    }
  }
  return volume;
}

function buildEdges(path) {
  let r = 0,
    c = 0;
  const edges = [];
  for (const {
    steps,
    delta: [rowDelta, colDelta],
  } of path) {
    let newR = r + steps * rowDelta,
      newC = c + steps * colDelta;
    if (r < newR || (r == newR && c < newC)) {
      edges.push([r, c, newR, newC]);
    } else {
      edges.push([newR, newC, r, c]);
    }
    r = newR;
    c = newC;
  }
  return edges;
}

function solve(path) {
  const edges = buildEdges(path);
  return findVolume(edges);
}

function problem1(plan) {
  return solve(
    plan.map(({ dir, steps }) => ({
      steps,
      delta: deltas.get(dir),
    }))
  );
}

function problem2(plan) {
  return solve(
    plan.map(({ color }) => ({
      steps: parseInt(color.slice(0, -1), 16),
      delta: deltas.get(color.at(-1)),
    }))
  );
}

const lines = readLines(process.argv[2]);
const plan = parse(lines);
console.log(problem1(plan));
console.log(problem2(plan));
