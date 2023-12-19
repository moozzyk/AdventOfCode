import { readLines, sum } from "../utils.js";

function parseRule(rule) {
  const regex = /(\w)(<|>)(\d+):(\w+)/g;

  if (rule.includes("<") || rule.includes(">")) {
    return [...rule.matchAll(regex)].map((parsed) => ({
      category: parsed[1],
      relation: parsed[2],
      value: Number(parsed[3]),
      action: parsed[4],
    }));
  }

  return { action: rule };
}
function parseWorkflows(lines) {
  const regex = /(\w+)\{(.+)\}/g;
  return new Map(
    lines
      .map((l) => [...l.matchAll(regex)])
      .filter((l) => l.length > 0)
      .map(([[_, id, rules]]) => [id, rules.split(",").map(parseRule).flat()])
  );
}

function parseParts(lines) {
  const regex = /\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}/g;
  return lines
    .map((l) => [...l.matchAll(regex)])
    .filter((l) => l.length > 0)
    .map(([[_, x, m, a, s]]) => ({
      x: Number(x),
      m: Number(m),
      a: Number(a),
      s: Number(s),
    }));
}

function evaluate({ category, relation, value, action }, part) {
  if (!relation) return action;
  if (relation == ">" && part[category] > value) return action;
  if (relation == "<" && part[category] < value) return action;
  return undefined;
}

function applyRules(part, rules, workflows) {
  for (const rule of rules) {
    const action = evaluate(rule, part);
    if (action === "R" || action === "A") {
      return action;
    }
    if (action) {
      return applyRules(part, workflows.get(action), workflows);
    }
  }
  throw new Error("No applicable rule found.");
}

function problem1(parts, workflows) {
  return sum(
    parts
      .map((p) => [p, applyRules(p, workflows.get("in"), workflows)])
      .filter(([_, result]) => result == "A")
      .map(([part, _]) => part.x + part.m + part.a + part.s)
  );
}

const lines = readLines(process.argv[2]);
const workflows = parseWorkflows(lines);
const parts = parseParts(lines);
console.log(problem1(parts, workflows));
