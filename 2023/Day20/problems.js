import { readLines } from "../utils.js";

function findInputs(conjucntion, config) {
  return [...config.entries()]
    .map(([k, v]) => (v.includes(conjucntion) ? k : undefined))
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
  const q = ["broadcaster"];
  let lowPulses = 1;
  let highPulses = 0;
  let qIdx = 0;
  while (qIdx < q.length) {
    const inputModule = q[qIdx++];
    const pulse = state.get(inputModule);
    for (const module of config.get(inputModule)) {
      if (pulse) {
        highPulses++;
      } else {
        lowPulses++;
      }
      if (!types.get(module)) continue;
      if (types.get(module).length == 0) {
        if (pulse) continue;
        state.set(module, !state.get(module));
      } else {
        state.set(
          module,
          !types.get(module).every((m) => state.get(m) === true)
        );
      }
      q.push(module);
    }
  }
  return [lowPulses, highPulses];
}

function problem1({ config, types }) {
  const state = new Map([...types.keys()].map((k) => [k, false]));
  let lowPulses = 0,
    highPulses = 0;
  for (let i = 0; i < 1000; i++) {
    const [l, h] = propagate(config, state, types);
    lowPulses += l;
    highPulses += h;
  }
  return lowPulses * highPulses;
}

const lines = readLines(process.argv[2]);
console.log(problem1(parse(lines)));
