import { readLines } from "../utils.js";

function predict(lastPrediction) {
  if (lastPrediction.every((n) => n == 0)) {
    return { front: 0, back: 0 };
  }

  const newPrediction = [];
  for (let i = 0; i < lastPrediction.length - 1; i++) {
    newPrediction.push(lastPrediction[i + 1] - lastPrediction[i]);
  }
  const { front, back } = predict(newPrediction);
  return {
    front: lastPrediction.at(0) - front,
    back: lastPrediction.at(-1) + back,
  };
}

function solve(history) {
  return history
    .map((h) => predict(h))
    .reduce(
      (acc, n) => ({ front: acc.front + n.front, back: acc.back + n.back }),
      { front: 0, back: 0 }
    );
}

const regex = /-?\d+/g;
const history = readLines(process.argv[2]).map((l) =>
  l.match(regex).map(Number)
);
const result = solve(history);
console.log(result.front);
console.log(result.back);
