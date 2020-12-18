#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

std::vector<std::string> read_equations(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::string line;
  std::vector<std::string> lines;
  while (std::getline(file, line)) {
    lines.push_back(line);
  }
  return lines;
}

long evaluate(const std::string& equation, size_t& i) {
  long value = 0;
  char op = '+';
  while (i < equation.size()) {
    char c = equation[i];
    i++;
    switch (c) {
      case ' ':
        break;
      case '*':
      case '+':
        op = c;
        break;
      case '(':
        if (op == '*') {
          value *= evaluate(equation, i);
        } else if (op == '+') {
          value += evaluate(equation, i);
        } else {
          throw std::logic_error("Unexpected op");
        }
        break;
      case ')':
        return value;
      default:
        if (op == '*') {
          value *= c - '0';
        } else if (op == '+') {
          value += c - '0';
        } else {
          throw std::logic_error("Unexpected op");
        }
    }
  }
  return value;
}

long problem1(const std::vector<std::string>& equations) {
  long result = 0;
  for (const auto& e : equations) {
    size_t i = 0;
    result += evaluate(e, i);
  }
  return result;
}

int main(int argc, const char* argv[]) {
  auto equations = read_equations("input.txt");
  std::cout << problem1(equations) << std::endl;
  return 0;
}