#include <algorithm>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <unordered_set>
#include <vector>

std::vector<std::string> read_lines(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::vector<std::string> lines;
  std::string tmp;
  while (file >> tmp) {
    lines.push_back(tmp);
  }
  return lines;
}

enum Direction {
  E,
  N,
  W,
  S
};

int problem1(const std::vector<std::string>& lines) {
  int x = 0, y = 0;
  int direction = E;

  for (const auto& l : lines) {
    int n = std::stoi(l.substr(1));
    char prefix = l[0];
    if (prefix == 'F') {
      prefix = direction;
    }
    std::cout << l << " " << n << std::endl;
    switch (prefix) {
      case 'R':
        direction = (4 + direction - n / 90) % 4;
        break;
      case 'L':
        direction = (direction + n / 90) % 4;
        break;
      case N:
      case 'N':
        y += n;
        break;
      case S:
      case 'S':
        y -= n;
        break;
      case E:
      case 'E':
        std::cout << "#";
        x += n;
        break;
      case W:
      case 'W':
        x -= n;
        break;
    }
  }

  return std::abs(x) + std::abs(y);
}

int main(int argc, const char* argv[]) {
  auto lines = read_lines("input.txt");
  std::cout << problem1(lines) << std::endl;
  return 0;
}
