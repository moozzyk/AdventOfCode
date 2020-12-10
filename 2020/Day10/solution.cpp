#include <algorithm>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <unordered_set>
#include <vector>

std::vector<int> read_numbers(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::vector<int> numbers;
  unsigned long tmp;
  while (file >> tmp) {
    numbers.push_back(tmp);
  }
  return numbers;
}

int problem1(const std::vector<int>& n) {
  auto numbers = n;
  numbers.push_back(0);
  std::sort(numbers.begin(), numbers.end());

  int ones = 0, threes = 1;
  for (auto i = 0; i < numbers.size() - 1; i++) {
    auto difference = numbers[i + 1] - numbers[i];
    if (difference == 1) {
      ones++;
    } else if (difference == 3) {
      threes++;
    }
  }
  return ones * threes;
}

unsigned long problem2(const std::vector<int>& numbers) {
  std::unordered_set<int> adapters;
  for (auto n : numbers) {
    adapters.insert(n);
  }
  auto target = *(std::max_element(numbers.begin(), numbers.end()));
  std::vector<unsigned long> dp(target + 1, 0);
  dp[0] = 1;
  for (auto i = 1; i <= target; i++) {
    if (adapters.find(i) != adapters.end()) {
      dp[i] = dp[i - 1];
      if (i > 1) {
        dp[i] += dp[i - 2];
      }
      if (i > 2) {
        dp[i] += dp[i - 3];
      }
    }
  }
  return dp.back();
}

int main(int argc, const char* argv[]) {
  auto numbers = read_numbers("input.txt");
  std::cout << problem1(numbers) << std::endl;
  std::cout << problem2(numbers) << std::endl;
  return 0;
}
