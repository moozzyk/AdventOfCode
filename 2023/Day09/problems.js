import { readLines } from "../utils.js";

function predict(predictions) {
  if (predictions.at(-1).every((n) => n == 0)) {
    return 0;
  }

  const lastPrediction = predictions.at(-1);
  const newPrediction = [];
  for (let i = 0; i < lastPrediction.length - 1; i++) {
    newPrediction.push(lastPrediction[i + 1] - lastPrediction[i]);
  }
  predictions.push(newPrediction);
  return lastPrediction.at(-1) + predict(predictions);
}

function problem1(history) {
  return history.map((h) => predict([h])).reduce((acc, n) => acc + n, 0);
}

const regex = /-?\d+/g;
const history = readLines(process.argv[2]).map((l) =>
  l.match(regex).map(Number)
);
console.log(problem1(history));
