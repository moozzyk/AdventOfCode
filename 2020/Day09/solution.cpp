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

unsigned long problem2(const std::vector<unsigned long>& numbers, unsigned long target) {
  int l = 0, r = 0;
  long sum = numbers[0];

  while (sum != target) {
    if (sum < target) {
      r++;
      sum += numbers[r];
    } else if (sum > target) {
      sum -= numbers[l];
      l++;
    }
  }

  unsigned long minimum = numbers[l], maximum = numbers[l];
  for (auto i = l; i < r; i++) {
    minimum = std::min(minimum, numbers[i]);
    maximum = std::max(maximum, numbers[i]);
  }
  return minimum + maximum;
}

int main(int argc, const char* argv[]) {
  auto numbers = read_numbers("input.txt");
  auto target = problem1(numbers);
  std::cout << target << std::endl;
  std::cout << problem2(numbers, target) << std::endl;
  return 0;
}
