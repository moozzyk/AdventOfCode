#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

int problem1(const std::vector<int>& numbers) {
  std::unordered_map<int, std::vector<int>> turns;
  int turn = 1;
  for (auto n : numbers) {
    turns[n] = {turn++};
  }

  auto last = 6;
  for (; turn <= 2020; turn++) {
    auto& prev = turns[last];
    if (prev.size() > 1) {
      last = prev.back() - prev[prev.size() - 2];
    } else {
      last = 0;
    }
    turns[last].push_back(turn);
  }

  return last;
}

int main(int argc, const char* argv[]) {
  auto input = std::vector<int>{15, 12, 0, 14, 3, 1};
  std::cout << problem1(input) << std::endl;
  return 0;
}
