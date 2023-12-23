import { readLines } from "../utils.js";

function sort(cubes) {
  cubes.sort((c1, c2) => c1[0].z - c2[0].z);
}

function parse(lines) {
  return lines.map((l) =>
    l
      .split("~")
      .map((c) => c.split(",").map(Number))
      .map(([x, y, z]) => ({ x, y, z }))
  );
}

function isOverlapping(c1s, c1e, c2s, c2e) {
  return !(c1s > c2e || c1e < c2s);
}

function isClashing(c1, c2) {
  return (
    isOverlapping(c1[0].x, c1[1].x, c2[0].x, c2[1].x) &&
    isOverlapping(c1[0].y, c1[1].y, c2[0].y, c2[1].y) &&
    isOverlapping(c1[0].z, c1[1].z, c2[0].z, c2[1].z)
  );
}

function settle() {
  nextCube: for (let i = 0; i < cubes.length; i++) {
    const candidate = [{ ...cubes[i][0] }, { ...cubes[i][1] }];
    while (candidate[0].z > 1) {
      candidate[0].z--;
      candidate[1].z--;
      for (let j = i - 1; j >= 0; j--) {
        if (isClashing(candidate, cubes[j])) {
          candidate[0].z++;
          candidate[1].z++;
          continue nextCube;
        }
      }
      cubes[i] = candidate;
    }
  }
  sort(cubes);
}

function disintegrationCandidates(cubes) {
  const supportMap = new Map();
  const supportAny = new Set();
  for (let i = 0; i < cubes.length; i++) {
    supportMap.set(i, []);
    const candidate = [{ ...cubes[i][0] }, { ...cubes[i][1] }];
    candidate[0].z--;
    candidate[1].z--;
    for (let j = i - 1; j >= 0; j--) {
      if (isClashing(candidate, cubes[j])) {
        supportMap.get(i).push(j);
        supportAny.add(j);
      }
    }
  }

  const redundantSupport = new Set();
  for (const v of supportMap.values()) {
    if (v.length > 1) {
      v.forEach((e) => redundantSupport.add(e));
    }
  }

  for (const v of supportMap.values()) {
    if (v.length == 1) {
      redundantSupport.delete(v[0]);
    }
  }
  return cubes.length - supportAny.size + redundantSupport.size;
}

function problem1(cubes) {
  settle(cubes);
  return disintegrationCandidates(cubes);
}

const lines = readLines(process.argv[2]);
const cubes = parse(lines).sort((c1, c2) => c1[0].z - c2[0].z);
console.log(problem1(cubes));
