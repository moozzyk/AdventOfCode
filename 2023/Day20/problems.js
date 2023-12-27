import { readLines, lcm } from "../utils.js";

function findInputs(conjunction, config) {
  return [...config.entries()]
    .map(([k, v]) => (v.includes(conjunction) ? k : undefined))
    .filter((e) => e);
}

function parse(lines) {
  const tmp = lines
    .map((l) => l.split(" -> "))
    .map(([src, dst]) => [src, dst.split(", ")]);

  const config = new Map(
    tmp.map(([src, dst]) => [src.replaceAll(/[%&]/g, ""), dst])
  );

  const types = new Map(
    tmp
      .filter(([src]) => src != "broadcaster")
      .map(([src]) => [src.substring(1), src[0]])
      .map(([name, type]) => [
        name,
        type == "&" ? findInputs(name, config) : [],
      ])
  );

  return { config, types };
}

function propagate(config, state, types) {
  const highConjunctors = new Set();
  const q = config.get("broadcaster").map((m) => [m, false]);
  let lowPulses = 1;
  let highPulses = 0;
  let qIdx = 0;
  while (qIdx < q.length) {
    const [module, pulse] = q[qIdx++];
    if (pulse) {
      highPulses++;
    } else {
      lowPulses++;
    }
    if (!types.get(module)) continue; // rx
    if (types.get(module).length == 0) {
      if (pulse) continue;
      state.set(module, !state.get(module));
    } else {
      state.set(module, !types.get(module).every((m) => state.get(m) === true));
      if (state.get(module)) {
        highConjunctors.add(module);
      }
    }

    for (const nextModule of config.get(module)) {
      q.push([nextModule, state.get(module)]);
    }
  }
  return { lowPulses, highPulses, highConjunctors };
}

function problem1({ config, types }) {
  const state = new Map([...types.keys()].map((k) => [k, false]));
  let lowPulses = 0,
    highPulses = 0;
  for (let i = 0; i < 1000; i++) {
    const result = propagate(config, state, types);
    lowPulses += result.lowPulses;
    highPulses += result.highPulses;
  }
  return lowPulses * highPulses;
}

function problem2({ config, types }) {
  const state = new Map([...types.keys()].map((k) => [k, false]));
  const collectors = new Map(
    findInputs(
      [...config.entries()].find(([k, v]) => v == "rx")[0],
      config
    ).map((m) => [m, []])
  );

  for (let i = 1; [...collectors.values()].some((v) => v.length == 0); i++) {
    const { highConjunctors } = propagate(config, state, types);

    for (const [m, v] of collectors) {
      if (highConjunctors.has(m)) {
        v.push(i);
      }
    }
  }

  return lcm([...collectors.values()].flat());
}

const lines = readLines(process.argv[2]);
console.log(problem1(parse(lines)));
console.log(problem2(parse(lines)));
