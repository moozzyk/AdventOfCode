#include <algorithm>
#include <deque>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

int solve(const std::vector<int>& numbers, int max_turn) {
  std::unordered_map<int, std::deque<int>> turns;

  int turn = 1;
  for (auto n : numbers) {
    turns[n] = {turn++};
  }

  auto last = numbers.back();
  for (; turn <= max_turn; turn++) {
    auto& prev = turns[last];
    if (prev.size() > 1) {
      last = prev.back() - prev.front();
      prev.pop_front();
    } else {
      last = 0;
    }
    turns[last].push_back(turn);
  }
  return last;
}

int problem1(const std::vector<int>& numbers) {
  return solve(numbers, 2020);
}

int problem2(const std::vector<int>& numbers) {
  return solve(numbers, 30000000);
}

int main(int argc, const char* argv[]) {
  auto input = std::vector<int>{15, 12, 0, 14, 3, 1};
  std::cout << problem1(input) << std::endl;
  std::cout << problem2(input) << std::endl;
  return 0;
}
