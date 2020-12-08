#include <fstream>
#include <iostream>
#include <unordered_set>
#include <vector>

std::vector<int> read_numbers(const std::string &file_name) {
  std::ifstream file;
  file.open(file_name);
  return std::vector<int>{std::istream_iterator<int>(file), {}};
}

std::pair<int, int> find_sum(const std::vector<int> &input, int target) {
  std::unordered_set<int> numbers;
  for (const auto n : input) {
    if (numbers.find(target - n) != numbers.end()) {
      return {n, (target - n)};
    }
    numbers.insert(n);
  }

  return {-1, -1};
}

int problem1(const std::vector<int> &input) {
  auto pair = find_sum(input, 2020);
  return pair.first * pair.second;
}

int problem2(const std::vector<int> &input) {
  for (const auto m : input) {
    auto target = 2020 - m;
    auto pair = find_sum(input, target);
    if (pair.first != -1) {
      return m * pair.first * pair.second;
    }
  }
  return -1;
}

int main(int argc, const char *argv[]) {
  auto numbers = read_numbers("input.txt");
  std::cout << problem1(numbers) << std::endl;
  std::cout << problem2(numbers) << std::endl;
  return 0;
}