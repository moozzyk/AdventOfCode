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
  std::vector<int> seen(max_turn + 1, -1);

  int turn = 1;
  for (; turn < numbers.size(); turn++) {
    seen[numbers[turn - 1]] = turn;
  }

  auto last = numbers.back();
  for (; turn < max_turn; turn++) {
    if (seen[last] == -1) {
      seen[last] = turn;
      last = 0;
    } else {
      auto d = turn - seen[last];
      seen[last] = turn;
      last = d;
    }
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
