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

unsigned long evaluate1(const std::string& equation, size_t& i) {
  unsigned long value = 0;
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
          value *= evaluate1(equation, i);
        } else if (op == '+') {
          value += evaluate1(equation, i);
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

unsigned long evaluate2(const std::string& equation, size_t& i) {
  std::vector<unsigned long> values{};
  char op = '*';

  while (i < equation.size()) {
    char c = equation[i];
    i++;
    switch (c) {
      case ' ':
        break;
      case '*':
        op = c;
        break;
      case '+':
        op = c;
        break;
      case '(':
        if (op == '+') {
          auto tmp = values.back();
          values.pop_back();
          values.push_back(tmp + evaluate2(equation, i));
        } else if (op == '*') {
          values.push_back(evaluate2(equation, i));
        } else {
          throw std::logic_error("Unexptected operator");
        }
        break;
      case ')':
        return std::accumulate(values.begin(), values.end(), 1UL, [](auto x, auto y) { return x * y; });
      default:
        if (op == '+') {
          auto tmp = values.back();
          values.pop_back();
          values.push_back(tmp + (c - '0'));
        } else if (op == '*') {
          values.push_back(c - '0');
        } else {
          throw std::logic_error("Unexpected operator");
        }
    }
  }

  return std::accumulate(values.begin(), values.end(), 1UL, [](auto x, auto y) { return x * y; });
}
unsigned long run(
    const std::vector<std::string>& equations,
    std::function<unsigned long(const std::string&, size_t&)> eval) {
  return std::accumulate(
      equations.begin(),
      equations.end(),
      0UL,
      [eval](unsigned long acc, const auto& e) {
        size_t i = 0;
        return acc + eval(e, i);
      });
}

unsigned long problem1(const std::vector<std::string>& equations) {
  return run(equations, evaluate1);
}

unsigned long problem2(const std::vector<std::string>& equations) {
  return run(equations, evaluate2);
}

int main(int argc, const char* argv[]) {
  auto equations = read_equations("input.txt");
  std::cout << problem1(equations) << std::endl;
  std::cout << problem2(equations) << std::endl;
  return 0;
}