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

std::vector<instruction> read_input(const std::string& file_name) {
  std::ifstream file;
  char discard;
  std::string operation, mask;
  long address, value;
  file.open(file_name);
  std::vector<instruction> result;
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
long sum_memory(const std::unordered_map<long, long>& memory) {
  return std::accumulate(
      memory.begin(),
      memory.end(),
      0L,
      [](long acc, const auto& item) {
        return acc + item.second;
      });
}

long apply_mask(long value, const std::string& mask) {
  std::string value_bin = std::bitset<36>(value).to_string();
  std::string result;
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
  return sum_memory(memory);
}

void write_to_memory(
    const std::string& mask, const std::string& address, int idx,
    long effective_address, long value, std::unordered_map<long, long>& memory) {
  if (idx == mask.size()) {
    memory[effective_address] = value;
    return;
  }
  auto bit = 0;
  switch (mask[idx]) {
    case '0':
      bit = static_cast<int>(address[idx] == '1');
      write_to_memory(mask, address, idx + 1, (effective_address << 1) | bit, value, memory);
      break;
    case '1':
      write_to_memory(mask, address, idx + 1, (effective_address << 1) | 1, value, memory);
      break;
    case 'X':
      write_to_memory(mask, address, idx + 1, (effective_address << 1) | 1, value, memory);
      write_to_memory(mask, address, idx + 1, (effective_address << 1), value, memory);
      break;
  }
}

long problem2(const std::vector<instruction>& instructions) {
  std::unordered_map<long, long> memory;
  std::string current_mask;
  for (auto& [operation, address, value, mask] : instructions) {
    if (operation == "mask") {
      current_mask = mask;
    } else {
      auto address_bin = std::bitset<36>(address).to_string();
      write_to_memory(current_mask, address_bin, 0, 0, value, memory);
    }
  }
  return sum_memory(memory);
}

int main(int argc, const char* argv[]) {
  auto input = read_input("input.txt");
  std::cout << problem1(input) << std::endl;
  std::cout << problem2(input) << std::endl;
  return 0;
}
