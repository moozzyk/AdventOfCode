import { readLines, isDigit } from "../utils.js";

function parse(lines) {
  let seeds = lines[0].split(" ").slice(1).map(Number);
  const almanac = [];
  const buffer = [];
  for (const line of lines.slice(2)) {
    if (!isDigit(line[0])) {
      almanac.push([...buffer]);
      buffer.length = 0;
    } else {
      buffer.push(line.split(" ").map(Number));
    }
  }
  almanac.push([...buffer]);
  return { seeds, almanac };
}

function findLocation(seed, almanac) {
  for (let i = 0; i < almanac.length; i++) {
    for (let [dest, src, length] of almanac[i]) {
      if (seed >= src && seed < src + length) {
        seed = dest + (seed - src);
        break;
      }
    }
  }
  return seed;
}

function problem1({ seeds, almanac }) {
  return Math.min(...seeds.map((s) => findLocation(s, almanac)));
}

function handleCategory({ seedStart, seedEnd }, almanac) {
  for (let [dst, src, length] of almanac) {
    if (seedEnd < src || seedStart >= src + length) {
      continue;
    }
    if (seedStart >= src && seedEnd < src + length) {
      return [
        {
          seedStart: dst + (seedStart - src),
          seedEnd: dst + (seedEnd - src),
        },
      ];
    }
    if (seedStart < src && seedEnd >= src + length) {
      return [
        ...handleCategory(
          {
            seedStart,
            seedEnd: src - 1,
          },
          almanac
        ),
        { seedStart: dst, seedEnd: dst + length - 1 },
        ...handleCategory({ seedStart: src + length, seedEnd }, almanac),
      ];
    }
    if (seedStart < src) {
      return [
        { seedStart: dst, seedEnd: dst + (seedEnd - src) },
        ...handleCategory(
          {
            seedStart,
            seedEnd: src - 1,
          },
          almanac
        ),
      ];
    }
    if (seedEnd >= src + length) {
      return [
        {
          seedStart: dst + (seedStart - src),
          seedEnd: dst + length - 1,
        },
        ...handleCategory(
          {
            seedStart: src + length,
            seedEnd,
          },
          almanac
        ),
      ];
    }
    console.error("Logic error");
  }
  return [{ seedStart, seedEnd }];
}

function findLocationRange(seeds, almanac) {
  for (const a of almanac) {
    let newSeeds = [];
    for (const s of seeds) {
      newSeeds = [...newSeeds, ...handleCategory(s, a)];
    }
    seeds = [...newSeeds];
  }
  return seeds;
}

function problem2({ seeds, almanac }) {
  const result = [];
  for (let i = 0; i < seeds.length; i += 2) {
    const r = findLocationRange(
      [{ seedStart: seeds[i], seedEnd: seeds[i] + seeds[i + 1] - 1 }],
      almanac
    );
    result.push(r);
  }
  return Math.min(...result.flat().map(({ seedStart }) => seedStart));
}

const { seeds, almanac } = parse(readLines(process.argv[2]));
console.log(problem1({ seeds: seeds, almanac }));
console.log(problem2({ seeds: seeds, almanac }));
