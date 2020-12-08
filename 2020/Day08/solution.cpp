#include <algorithm>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <unordered_set>
#include <vector>

using instruction = std::pair<std::string, int>;

std::vector<instruction> read_program(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::vector<instruction> program;
  instruction i;
  while (file >> i.first >> i.second) {
    program.push_back(i);
  }
  return program;
}

int problem1(const std::vector<instruction>& program) {
  int acc = 0;
  int ip = 0;
  std::unordered_set<int> executed_lines;

  while (true) {
    if (executed_lines.find(ip) != executed_lines.end()) {
      return acc;
    }
    executed_lines.insert(ip);
    auto [opcode, arg] = program[ip];
    if (opcode == "jmp") {
      ip += arg;
    } else if (opcode == "acc") {
      acc += arg;
      ip++;
    } else if (opcode == "nop") {
      ip++;
    }
  }
}

int main(int argc, const char* argv[]) {
  auto bags = read_program("input.txt");
  std::cout << problem1(bags) << std::endl;
  // std::cout << problem2(bags) << std::endl;
  return 0;
}
