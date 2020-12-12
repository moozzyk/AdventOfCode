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
  W,
  N,
  E,
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
    switch (prefix) {
      case 'L':
        direction = (4 + direction - n / 90) % 4;
        break;
      case 'R':
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

int problem2(const std::vector<std::string>& lines) {
  int w_x = 10, w_y = 1;
  int s_x = 0, s_y = 0;

  for (const auto& l : lines) {
    int n = std::stoi(l.substr(1));
    char prefix = l[0];
    switch (prefix) {
      case 'F':
        s_x += n * w_x;
        s_y += n * w_y;
        break;
      case 'L':
        n = 360 - n;
      case 'R':
        for (int i = 0; i < n / 90; i++) {
          int tmp = w_x;
          w_x = w_y;
          w_y = -tmp;
        }
        break;
      case 'N':
        w_y += n;
        break;
      case 'S':
        w_y -= n;
        break;
      case 'E':
        w_x += n;
        break;
      case 'W':
        w_x -= n;
        break;
    }
  }
  return std::abs(s_x) + std::abs(s_y);
}

int main(int argc, const char* argv[]) {
  auto lines = read_lines("input.txt");
  std::cout << problem1(lines) << std::endl;
  std::cout << problem2(lines) << std::endl;
  return 0;
}
