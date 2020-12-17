#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using layer = std::vector<std::string>;

std::vector<std::string> read_input(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::string line;
  std::vector<std::string> input;
  while (file >> line) {
    input.push_back(line);
  }
  return input;
}

int num_active(const layer& l, int row, int col) {
  int result = 0;
  for (auto x : {-1, 0, 1}) {
    for (auto y : {-1, 0, 1}) {
      auto r1 = row + x;
      auto c1 = col + y;
      if (r1 < 0 || c1 < 0 || r1 >= l.size() || c1 >= l.size()) {
        continue;
      }
      if (l[r1][c1] == '#') {
        result++;
      }
    }
  }
  return result;
}

layer calculate_new_layer(const layer& upper, const layer& mid, const layer& lower) {
  auto new_layer = layer(mid.size() + 2, std::string(mid[0].size() + 2, '.'));
  for (auto row = 0; row < mid.size(); ++row) {
    for (auto col = 0; col < mid[0].size(); ++col) {
      auto active = num_active(upper, row, col) +
                    num_active(mid, row, col) +
                    num_active(lower, row, col);
      if (mid[row][col] == '#') {
        active--;
        if (active == 2 || active == 3) {
          new_layer[row + 1][col + 1] = '#';
        }
      } else if (active == 3) {
        new_layer[row + 1][col + 1] = '#';
      }
    }
  }
  return new_layer;
}

std::vector<std::vector<std::string>> cycle(std::vector<layer> cube) {
  for (auto& l : cube) {
    for (auto& r : l) {
      r.insert(0, ".");
      r.append(".");
    }
    l.insert(l.begin(), std::string(l[0].size(), '.'));
    l.push_back(std::string(l[0].size(), '.'));
  }

  std::vector<std::vector<std::string>> new_cube;
  layer dummy = layer(cube[0].size(), std::string(cube[0][0].size(), '.'));

  new_cube.push_back(calculate_new_layer(dummy, dummy, cube.back()));
  for (int i = 0; i < cube.size(); i++) {
    const auto& lower = i == 0 ? dummy : cube[i - 1];
    const auto& mid = cube[i];
    const auto& upper = i == cube.size() - 1 ? dummy : cube[i + 1];
    new_cube.push_back(calculate_new_layer(lower, mid, upper));
  }
  new_cube.push_back(calculate_new_layer(cube.back(), dummy, dummy));
  return new_cube;
}

void print_cube(const std::vector<layer>& cube) {
  for (int z = 0; z < cube.size(); ++z) {
    std::cout << "Z=" << z << std::endl;
    for (const auto& l : cube[z]) {
      std::cout << l << std::endl;
    }
    std::cout << std::endl;
  }
  std::cout << "=============" << std::endl;
}

int count_active(const std::vector<layer>& cube) {
  int count = 0;
  for (const auto& l : cube) {
    for (const auto& r : l) {
      count += std::count(r.begin(), r.end(), '#');
    }
  }
  return count;
}

int problem1(const layer& initial) {
  std::vector<std::vector<std::string>> cube = {initial};
  print_cube(cube);
  for (int i = 0; i < 6; ++i) {
    cube = cycle(cube);
  }
  return count_active(cube);
}

int main(int argc, const char* argv[]) {
  auto input = read_input("input.txt");
  std::cout << problem1(input) << std::endl;
  return 0;
}
