import { readLines } from "../utils.js";

function predict(predictions) {
  if (predictions.at(-1).every((n) => n == 0)) {
    return { front: 0, back: 0 };
  }

  const lastPrediction = predictions.at(-1);
  const newPrediction = [];
  for (let i = 0; i < lastPrediction.length - 1; i++) {
    newPrediction.push(lastPrediction[i + 1] - lastPrediction[i]);
  }
  predictions.push(newPrediction);
  const { front, back } = predict(predictions);
  return {
    front: lastPrediction.at(0) - front,
    back: lastPrediction.at(-1) + back,
  };
}

function problem1(history) {
  return history
    .map((h) => predict([h]))
    .map((p) => p.back)
    .reduce((acc, n) => acc + n, 0);
}

function problem2(history) {
  return history
    .map((h) => predict([h]))
    .map((p) => p.front)
    .reduce((acc, n) => acc + n, 0);
}

const regex = /-?\d+/g;
const history = readLines(process.argv[2]).map((l) =>
  l.match(regex).map(Number)
);
console.log(problem1(history));
console.log(problem2(history));
