import { readLines } from "../utils.js";

function createGraph(lines) {
  const input = lines
    .map((l) => l.split(": "))
    .map(([from, tos]) => [...tos.split(" ").map((to) => [from, to])])
    .flat();

  const graph = new Map();
  for (const [from, to] of input) {
    if (!graph.has(from)) graph.set(from, new Set());
    if (!graph.has(to)) graph.set(to, new Set());
    graph.get(from).add(to);
    graph.get(to).add(from);
  }

  return graph;
}

function visit(startNode, graph) {
  const q = [startNode];
  let qIdx = 0;
  const visited = new Set();
  while (qIdx < q.length) {
    const node = q[qIdx++];
    visited.add(node);
    for (const n of graph.get(node)) {
      if (!visited.has(n)) {
        q.push(n);
      }
    }
  }
  return visited;
}

function getPath(start, end, graph, visitedEdges) {
  var q = [[start, [start]]];
  var qIdx = 0;
  var visitedNodes = new Set();
  while (qIdx < q.length) {
    var [v, path] = q[qIdx++];
    visitedNodes.add(v);
    if (v == end) {
      return path;
    }
    for (const n of graph.get(v)) {
      if (visitedNodes.has(n) || visitedEdges.has(`${v}_${n}`)) continue;
      q.push([n, [...path, n]]);
    }
  }
  return undefined;
}

function uniquePathEdges(start, end, graph) {
  const visitedEdges = new Set();
  let numPaths = 0;
  while (true) {
    const path = getPath(start, end, graph, visitedEdges);
    if (!path) {
      break;
    }
    numPaths++;
    for (let i = 0; i < path.length - 1; i++) {
      visitedEdges.add(`${path[i]}_${path[i + 1]}`);
      visitedEdges.add(`${path[i + 1]}_${path[i]}`);
    }
  }
  if (numPaths == 3) {
    return visitedEdges;
  }
  return undefined;
}

function intersect(bridges, candidateBridges) {
  for (const b of bridges) {
    if (!candidateBridges.has(b)) {
      bridges.delete(b);
    }
  }
}

function problem1(lines) {
  const graph = createGraph(lines);

  const minCut = new Set(
    [...graph.entries()]
      .map(([from, tos]) => [...tos].map((to) => `${from}_${to}`))
      .flat()
  );
  let i = 0;
  for (let startNode of graph.keys()) {
    for (let endNode of graph.keys()) {
      if (startNode != endNode) {
        const candidates = uniquePathEdges(startNode, endNode, graph);
        if (candidates) {
          intersect(minCut, candidates);
        }
      }
      if (minCut.size == 6) {
        const n = graph.keys().next().value;
        const nodeCount = visit(n, graph).size;
        for (const edge of minCut) {
          const [from, to] = edge.split("_");
          graph.get(from).delete(to);
        }
        const componentCount = visit(n, graph).size;
        return componentCount * (nodeCount - componentCount);
      }
    }
  }
}

const lines = readLines(process.argv[2]);
console.log(problem1(lines));
