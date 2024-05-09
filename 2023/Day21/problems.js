import { readLines } from "../utils.js";

function printGardenTile(garden, visited) {
  for (let r = 0; r < garden.length; r++) {
    const line = [];
    for (let c = 0; c < garden.length; c++) {
      const posHash = `${r}_${c}`;
      if (visited.has(posHash)) {
        line.push("*");
      } else {
        line.push(garden[r][c]);
      }
    }
    console.log(line.join(""));
  }
  console.log();
}

function print(maxSteps, visited, tileSize) {
  const rows = [];
  for (let r = -(maxSteps + 2); r <= maxSteps + 2; r++) {
    const row = [];
    for (let c = -(maxSteps + 2); c <= maxSteps + 2; c++) {
      const key = `${r}_${c}`;
      if (visited.has(key) && visited.get(key) % 2 == maxSteps % 2) {
        row.push("*");
      } else {
        row.push(".");
      }
      if (Math.abs(c - Math.floor(tileSize / 2)) % tileSize == 0) {
        row.push(" ");
      }
    }

    rows.push(row);
    if (Math.abs(r - Math.floor(tileSize / 2)) % tileSize == 0) {
      rows.push([]);
    }
  }
  for (const r of rows) {
    console.log(r.join(""));
  }
  console.log("---------");
}

function findStart(garden) {
  for (let r = 0; r < garden.length; r++) {
    for (let c = 0; c < garden[0].length; c++) {
      if (garden[r][c] == "S") {
        return { r, c };
      }
    }
  }
  throw new Error("Couldn't find starting pont.");
}

function visitGardenTile(garden, maxSteps, rowStart, colStart) {
  const visited = new Map();
  const q = [{ r: rowStart, c: colStart, s: 0 }];
  let qIdx = 0;
  while (qIdx < q.length) {
    const { r, c, s } = q[qIdx++];
    if (r < 0 || r == garden.length || c < 0 || c == garden.length) {
      continue;
    }
    if (garden[r][c] == "#" || s > maxSteps) {
      continue;
    }
    const posHash = `${r}_${c}`;
    if (visited.has(posHash)) {
      continue;
    }
    visited.set(posHash, s);
    q.push({ r: r + 1, c, s: s + 1 });
    q.push({ r: r - 1, c, s: s + 1 });
    q.push({ r, c: c + 1, s: s + 1 });
    q.push({ r, c: c - 1, s: s + 1 });
  }
  return visited;
}

function problem1(garden, maxSteps) {
  const { r: startRow, c: startCol } = findStart(garden);
  const visited = visitGardenTile(garden, maxSteps, startRow, startCol);
  return [...visited.values()].filter((v) => v % 2 == maxSteps % 2).length;
}

function getTipOffset(gardenSize, steps) {
  return ((steps - (gardenSize + 1) / 2) % gardenSize) + 1;
}

function getFullTileRadius(gardenSize, maxSteps) {
  return Math.floor(maxSteps / gardenSize);
}

function getNumEvenTiles(radius) {
  const n = radius - (radius & 1);
  return n * n;
}

function getNumOddTiles(radius) {
  const n = radius - 1 + (radius & 1);
  return n * n;
}

function getNumVisitedSpots(visited) {
  let visitedEven = 0,
    visitedOdd = 0;
  for (const steps of visited.values()) {
    if (steps % 2 == 0) {
      visitedEven++;
    } else {
      visitedOdd++;
    }
  }
  return { visitedEven, visitedOdd };
}

function getNumVisitedEvenSpots(visited) {
  return getNumVisitedSpots(visited).visitedEven;
}

function getNumVisitedOddSpots(visited) {
  return getNumVisitedSpots(visited).visitedOdd;
}

function getNumVisitedSpotsOnFullTiles(garden, maxSteps) {
  const visited = visitGardenTile(garden, maxSteps, 0, 0);
  const { visitedEven, visitedOdd } = getNumVisitedSpots(visited);
  const radius = getFullTileRadius(garden.length, maxSteps);
  if (maxSteps % 2 == 1) {
    return (
      visitedEven * getNumEvenTiles(radius) +
      visitedOdd * getNumOddTiles(radius)
    );
  }
  return (
    visitedOdd * getNumEvenTiles(radius) + visitedEven * getNumOddTiles(radius)
  );
}

function getNumVisitedSpotsOnTipTiles(garden, maxSteps) {
  const radius = getFullTileRadius(garden.length, maxSteps);
  const numStepsToEdge =
    Math.max(0, radius - 1) * garden.length + Math.floor(garden.length / 2);
  const tipSteps = maxSteps - numStepsToEdge - 1;

  const visitGardenTileDir = (r, c) => visitGardenTile(garden, tipSteps, r, c);
  const midPos = Math.floor(garden.length / 2);

  const visitedRight = visitGardenTileDir(midPos, 0);
  const visitedLeft = visitGardenTileDir(midPos, garden.length - 1);
  const visitedUp = visitGardenTileDir(garden.length - 1, midPos);
  const visitedDown = visitGardenTileDir(0, midPos);

  if ((maxSteps + radius) % 2 == 1) {
    return (
      getNumVisitedEvenSpots(visitedRight) +
      getNumVisitedEvenSpots(visitedLeft) +
      getNumVisitedEvenSpots(visitedUp) +
      getNumVisitedEvenSpots(visitedDown)
    );
  }
  return (
    getNumVisitedOddSpots(visitedRight) +
    getNumVisitedOddSpots(visitedLeft) +
    getNumVisitedOddSpots(visitedUp) +
    getNumVisitedOddSpots(visitedDown)
  );
}

function getNumVisitedSpotsOnEdges(garden, maxSteps) {
  const gardenSize = garden.length;
  const tipOffset = getTipOffset(gardenSize, maxSteps);
  const steps1 = (tipOffset + Math.floor(gardenSize - 1) / 2) % gardenSize;
  const steps2 = steps1 + gardenSize;

  const visitedTR1 = visitGardenTile(garden, steps1, gardenSize - 1, 0);
  const visitedTR2 = visitGardenTile(garden, steps2, gardenSize - 1, 0);

  const visitedBR1 = visitGardenTile(garden, steps1, 0, 0);
  const visitedBR2 = visitGardenTile(garden, steps2, 0, 0);

  const visitedTL1 = visitGardenTile(
    garden,
    steps1,
    gardenSize - 1,
    gardenSize - 1
  );

  const visitedTL2 = visitGardenTile(
    garden,
    steps2,
    gardenSize - 1,
    gardenSize - 1
  );

  const visitedBL1 = visitGardenTile(garden, steps1, 0, gardenSize - 1);
  const visitedBL2 = visitGardenTile(garden, steps2, 0, gardenSize - 1);

  const radius = getFullTileRadius(garden.length, maxSteps);
  const num1Tiles = radius;
  const num2Tiles = radius - 1;

  if ((maxSteps + radius) % 2 == 1) {
    return (
      num1Tiles *
        (getNumVisitedEvenSpots(visitedTR1) +
          getNumVisitedEvenSpots(visitedBR1) +
          getNumVisitedEvenSpots(visitedTL1) +
          getNumVisitedEvenSpots(visitedBL1)) +
      num2Tiles *
        (getNumVisitedOddSpots(visitedTR2) +
          getNumVisitedOddSpots(visitedBR2) +
          getNumVisitedOddSpots(visitedTL2) +
          getNumVisitedOddSpots(visitedBL2))
    );
  }
  return (
    num1Tiles *
      (getNumVisitedOddSpots(visitedTR1) +
        getNumVisitedOddSpots(visitedBR1) +
        getNumVisitedOddSpots(visitedTL1) +
        getNumVisitedOddSpots(visitedBL1)) +
    num2Tiles *
      (getNumVisitedEvenSpots(visitedTR2) +
        getNumVisitedEvenSpots(visitedBR2) +
        getNumVisitedEvenSpots(visitedTL2) +
        getNumVisitedEvenSpots(visitedBL2))
  );
}

function problem2(garden, maxSteps) {
  return (
    getNumVisitedSpotsOnFullTiles(garden, maxSteps) +
    getNumVisitedSpotsOnTipTiles(garden, maxSteps) +
    getNumVisitedSpotsOnEdges(garden, maxSteps)
  );
}

const garden = readLines(process.argv[2]).map((r) => r.split(""));
console.log(problem1(garden, 64));
console.log(problem2(garden, 26501365));

// assumptions for problem 2:
// - tile are square
// - tile size is odd
// - there is straight passage from the center to tips
//    - makes all tip offsets the same
// - tips are located on the far edge of the tile
//    - as a consequence only tip tiles have two angles
// ^v<> - tip tiles
// aA bB cC dD - complementary edge tiles
// +- - same tile but visited spots are reverse
//
//          ^
//         d+a
//        D+-+A
//       d+-+-+a
//      <+-+-+-+>
//       c+-+-+b
//        C+-+B
//         c+b
//          v
