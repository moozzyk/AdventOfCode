#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using instruction = std::tuple<std::string, long, long, std::string>;
using instructions = std::vector<instruction>;

instructions read_input(const std::string& file_name) {
  std::ifstream file;
  char discard;
  std::string operation, mask;
  long address, value;
  file.open(file_name);
  instructions result;
  while (file >> discard >> discard) {
    if (discard == 'a') {
      operation = "mask";
      file >> discard >> discard >> discard >> mask;
      address = -1;
      value = -1;
    } else {
      operation = "mem";
      file >> discard >> discard >> address >> discard >> discard >> value;
      mask = "";
    }
    result.emplace_back(operation, address, value, mask);
  }
  return result;
}

long apply_mask(long value, const std::string& mask) {
  std::string value_bin = std::bitset<36>(value).to_string();
  std::string result;
  // std::cout << value << " " << value_bin << " " << mask << std::endl;
  for (int i = 0; i <= mask.size(); i++) {
    switch (mask[i]) {
      case 'X':
        result += value_bin[i];
        break;
      default:
        result += mask[i];
        break;
    }
  }
  // std::cout << result << " " << std::stol(result, 0, 2) << std::endl;
  return std::stol(result, 0, 2);
}

long problem1(const std::vector<instruction>& instructions) {
  std::unordered_map<long, long> memory;
  std::string current_mask = "";
  for (auto& [operation, address, value, mask] : instructions) {
    if (operation == "mask") {
      current_mask = mask;
    } else {
      memory[address] = apply_mask(value, current_mask);
    }
  }

  return std::accumulate(
      memory.begin(),
      memory.end(),
      0L,
      [](long acc, const auto& item) {
        return acc + item.second;
      });
}

int main(int argc, const char* argv[]) {
  auto input = read_input("input.txt");
  std::cout << problem1(input) << std::endl;
  return 0;
}
