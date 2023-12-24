import { readLines } from "../utils.js";

function parse(lines) {
  return lines
    .map((l) => l.split("@").map((l) => l.split(",").map(Number)))
    .map(([pos, velocity]) => ({
      pos,
      velocity,
    }));
}

function getIntersection(slope1, intercept1, slope2, intercept2) {
  if (slope1 == slope2) return { x: undefined, y: undefined };
  const x = (intercept2 - intercept1) / (slope1 - slope2);
  const y = slope1 * x + intercept1;
  return { x, y };
}

function getSlopeIntercept({ pos: [x, y], velocity: [vx, vy] }) {
  const slope = vy / vx;
  const intercept = y - slope * x;
  return [slope, intercept];
}

function problem1(hailstones, min, max) {
  let res = 0;
  for (let i = 0; i < hailstones.length; i++) {
    const [slope1, intercept1] = getSlopeIntercept(hailstones[i]);
    for (let j = i + 1; j < hailstones.length; j++) {
      const [slope2, intercept2] = getSlopeIntercept(hailstones[j]);
      const { x, y } = getIntersection(slope1, intercept1, slope2, intercept2);
      if (
        !(x === undefined || y === undefined) &&
        ((hailstones[i].velocity[1] < 0 && y < hailstones[i].pos[1]) ||
          (hailstones[i].velocity[1] > 0 && y > hailstones[i].pos[1])) &&
        ((hailstones[j].velocity[1] < 0 && y < hailstones[j].pos[1]) ||
          (hailstones[j].velocity[1] > 0 && y > hailstones[j].pos[1])) &&
        x >= min &&
        x <= max &&
        y >= min &&
        y <= max
      ) {
        res++;
      }
    }
  }
  return res;
}

const lines = readLines(process.argv[2]);
const hailstones = parse(lines);
console.log(problem1(hailstones, 200000000000000, 400000000000000));
