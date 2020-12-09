#include <algorithm>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <unordered_set>
#include <vector>

std::vector<unsigned long> read_numbers(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::vector<unsigned long> numbers;
  unsigned long tmp;
  while (file >> tmp) {
    numbers.push_back(tmp);
  }
  return numbers;
}

bool find_number(const std::vector<unsigned long>& numbers, size_t index) {
  std::cout << index - 1 << " " << index - 25 << std::endl;
  for (auto j = index - 25; j < index; ++j) {
    // underflow is ok
    auto target = numbers[index] - numbers[j];
    for (auto k = index - 25; k < index; ++k) {
      if (j != k && numbers[k] == target) {
        return true;
      }
    }
  }
  return false;
}

unsigned long problem1(const std::vector<unsigned long>& numbers) {
  for (auto i = 25; i < numbers.size(); i++) {
    if (!find_number(numbers, i)) {
      return numbers[i];
    }
  }
  throw std::logic_error("Solution not found");
}

int main(int argc, const char* argv[]) {
  auto numbers = read_numbers("input.txt");
  std::cout << problem1(numbers) << std::endl;
  return 0;
}
