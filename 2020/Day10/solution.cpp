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

int main(int argc, const char* argv[]) {
  auto numbers = read_numbers("input.txt");
  std::cout << problem1(numbers) << std::endl;
  return 0;
}
