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
  ["Q", 12],
  ["K", 13],
  ["A", 14],
]);

function parse(line) {
  const [hand, bid] = line.split(" ");
  return { hand, bid: Number(bid) };
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

function handTypeWithJokers(hand) {
  const handWithNoJokers = hand.replaceAll("J", "");
  const numJokers = hand.length - handWithNoJokers.length;
  if (numJokers === 0 || numJokers === 5) {
    return handType(hand);
  }

  var map = new Map();
  for (const c of handWithNoJokers) {
    map.set(c, (map.get(c) ?? 0) + 1);
  }

  const mostFreqCard = [...map.entries()].sort((a, b) => b[1] - a[1])[0][0];
  const strongestHandType = handWithNoJokers + mostFreqCard.repeat(numJokers);
  return handType(strongestHandType);
}

function compareHands(l, r, helperFn) {
  const hand1 = l.hand;
  const hand2 = r.hand;
  const hand1Type = helperFn(hand1);
  const hand2Type = helperFn(hand2);
  if (hand1Type != hand2Type) {
    return hand2Type - hand1Type;
  }
  for (let i = 0; i < hand1.length; i++) {
    if (hand1[i] === hand2[i]) continue;
    return labelMap.get(hand1[i]) - labelMap.get(hand2[i]);
  }
  return 0;
}

function solve(input, helperFn) {
  const comparer = (l, r) => compareHands(l, r, helperFn);
  return input
    .sort(comparer)
    .map(({ bid }, rank) => bid * (rank + 1))
    .reduce((acc, n) => acc + n, 0);
}

function problem1(input) {
  labelMap.set("J", 11);
  return solve(input, handType);
}

function problem2(input) {
  labelMap.set("J", 1);
  return solve(input, handTypeWithJokers);
}

const input = readLines(process.argv[2]).map(parse);
console.log(problem1(input));
console.log(problem2(input));
