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

std::pair<bool, int> execute(const std::vector<instruction>& program) {
  int acc = 0;
  int ip = 0;
  std::unordered_set<int> executed_lines;
  auto x = "0123456789";
  while (true) {
    if (ip >= program.size()) {
      return {true, acc};
    }
    if (executed_lines.find(ip) != executed_lines.end()) {
      return {false, acc};
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

int problem1(const std::vector<instruction>& program) {
  return execute(program).second;
}

int problem2(std::vector<instruction> program) {
  for (auto& line : program) {
    std::string save = line.first;
    if (line.first == "jmp") {
      line.first = "nop";
    } else if (line.first == "nop") {
      line.first = "jmp";
    }
    auto [result, acc] = execute(program);
    if (result) {
      return acc;
    }
    line.first = save;
  }
  throw std::logic_error("Unreachable code.");
}

int main(int argc, const char* argv[]) {
  auto program = read_program("input.txt");
  std::cout << problem1(program) << std::endl;
  std::cout << problem2(program) << std::endl;
  return 0;
}
