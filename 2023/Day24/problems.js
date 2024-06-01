import { readLines } from "../utils.js";
import { init } from "z3-solver";

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

async function problem2(h) {
  const { Context } = await init();
  const { Solver, Int } = new Context("main");

  const t1 = Int.const("t1");
  const t2 = Int.const("t2");
  const t3 = Int.const("t3");
  const rx = Int.const("rx");
  const rvx = Int.const("rvx");
  const ry = Int.const("ry");
  const rvy = Int.const("rvy");
  const rz = Int.const("rz");
  const rvz = Int.const("rvz");

  const solver = new Solver();
  solver.add(rx.add(t1.mul(rvx)).eq(t1.mul(h[0].velocity[0]).add(h[0].pos[0])));
  solver.add(ry.add(t1.mul(rvy)).eq(t1.mul(h[0].velocity[1]).add(h[0].pos[1])));
  solver.add(rz.add(t1.mul(rvz)).eq(t1.mul(h[0].velocity[2]).add(h[0].pos[2])));
  solver.add(rx.add(t2.mul(rvx)).eq(t2.mul(h[1].velocity[0]).add(h[1].pos[0])));
  solver.add(ry.add(t2.mul(rvy)).eq(t2.mul(h[1].velocity[1]).add(h[1].pos[1])));
  solver.add(rz.add(t2.mul(rvz)).eq(t2.mul(h[1].velocity[2]).add(h[1].pos[2])));
  solver.add(rx.add(t3.mul(rvx)).eq(t3.mul(h[2].velocity[0]).add(h[2].pos[0])));
  solver.add(ry.add(t3.mul(rvy)).eq(t3.mul(h[2].velocity[1]).add(h[2].pos[1])));
  solver.add(rz.add(t3.mul(rvz)).eq(t3.mul(h[2].velocity[2]).add(h[2].pos[2])));

  if ((await solver.check()) === "unsat") {
    throw new Error("Couldn't solve the problem.");
  }
  const model = solver.model();
  return model.get(rx).value() + model.get(ry).value() + model.get(rz).value();
}

const lines = readLines(process.argv[2]);
const hailstones = parse(lines);
console.log(problem1(hailstones, 200000000000000, 400000000000000));
// the solver takes a long time for 3 first lines, it can finish for fast the last 3
console.log(await problem2(hailstones.slice(-3)));

// Z3 leaves some handles open and node never exits
process.exit(0);
