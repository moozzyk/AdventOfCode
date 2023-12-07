import { readLines, isDigit } from "../utils.js";

const labelMap = new Map([
  ["2", 2],
  ["3", 3],
  ["4", 4],
  ["5", 5],
  ["6", 6],
  ["7", 7],
  ["8", 8],
  ["9", 9],
  ["T", 10],
  ["J", 11],
  ["Q", 12],
  ["K", 13],
  ["A", 14],
]);

function parse(line) {
  const [hand, bid] = line.split(" ");
  return { hand, bid };
}
function handType(hand) {
  var map = new Map();
  for (const c of hand) {
    map.set(c, (map.get(c) ?? 0) + 1);
  }

  const values = Array.from(map.values()).sort().reverse();

  if (values[0] == 5) return 0;
  if (values[0] == 4) return 1;
  if (values[0] == 3 && values[1] == 2) return 2;
  if (values[0] == 3) return 3;
  if (values[0] == 2 && values[1] == 2) return 4;
  if (values[0] == 2) return 5;
  return 6;
}

function compareHands(l, r) {
  const hand1 = l.hand;
  const hand2 = r.hand;
  const hand1Type = handType(hand1);
  const hand2Type = handType(hand2);
  if (hand1Type != hand2Type) {
    return hand2Type - hand1Type;
  }
  for (let i = 0; i < hand1.length; i++) {
    if (hand1[i] === hand2[i]) continue;
    return labelMap.get(hand1[i]) - labelMap.get(hand2[i]);
  }
  return 0;
}

function problem1(lines) {
  const input = lines.map(parse);
  return input
    .sort(compareHands)
    .map(({ bid }, rank) => bid * (rank + 1))
    .reduce((acc, n) => acc + n, 0);
}

const lines = readLines(process.argv[2]);
console.log(problem1(lines));
