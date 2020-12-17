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
    input.push_back(".........." + line + "..........");
  }
  std::string prototype = std::string(input[0].size(), '.');
  for (int i = 0; i < 10; i++) {
    input.insert(input.begin(), prototype);
    input.push_back(prototype);
  }
  return input;
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

layer calculate_new_layer(
    const layer& upper, const layer& mid, const layer& lower,
    const layer& prev_upper, const layer& prev_mid, const layer& prev_lower,
    const layer& next_upper, const layer& next_mid, const layer& next_lower) {
  auto new_layer = layer(mid.size() + 2, std::string(mid[0].size() + 2, '.'));
  for (auto row = 0; row < mid.size(); ++row) {
    for (auto col = 0; col < mid[0].size(); ++col) {
      auto active = num_active(upper, row, col) +
                    num_active(mid, row, col) +
                    num_active(lower, row, col) +
                    num_active(prev_upper, row, col) +
                    num_active(prev_mid, row, col) +
                    num_active(prev_lower, row, col) +
                    num_active(next_upper, row, col) +
                    num_active(next_mid, row, col) +
                    num_active(next_lower, row, col);
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

std::vector<layer> cycle(
    const std::vector<layer>& cube, const std::vector<layer>& prev_cube, const std::vector<layer>& next_cube) {
  std::vector<layer> new_cube;
  layer dummy = layer(cube[0].size(), std::string(cube[0][0].size(), '.'));

  if (next_cube.size() > 0) {
    new_cube.push_back(calculate_new_layer(
        dummy, dummy, cube.back(),
        dummy, dummy, prev_cube.back(),
        dummy, dummy, next_cube.back()));
  } else {
    new_cube.push_back(calculate_new_layer(
        dummy, dummy, cube.back(), {}, {}, {}, {}, {}, {}));
  }

  for (int i = 0; i < cube.size(); i++) {
    const auto& lower = i == 0 ? dummy : cube[i - 1];
    const auto& mid = cube[i];
    const auto& upper = i == cube.size() - 1 ? dummy : cube[i + 1];
    if (next_cube.size()) {
      const auto& prev_lower = i == 0 ? dummy : prev_cube[i - 1];
      const auto& prev_mid = prev_cube[i];
      const auto& prev_upper = i == cube.size() - 1 ? dummy : prev_cube[i + 1];
      const auto& next_lower = i == 0 ? dummy : next_cube[i - 1];
      const auto& next_mid = next_cube[i];
      const auto& next_upper = i == cube.size() - 1 ? dummy : next_cube[i + 1];
      new_cube.push_back(calculate_new_layer(
          lower, mid, upper,
          prev_lower, prev_mid, prev_upper,
          next_lower, next_mid, next_upper));
    } else {
      new_cube.push_back(calculate_new_layer(lower, mid, upper, {}, {}, {}, {}, {}, {}));
    }
  }

  if (next_cube.size() > 0) {
    new_cube.push_back(calculate_new_layer(
        cube.back(), dummy, dummy,
        prev_cube.back(), dummy, dummy,
        next_cube.back(), dummy, dummy));
  } else {
    new_cube.push_back(calculate_new_layer(
        cube.back(), dummy, dummy, {}, {}, {}, {}, {}, {}));
  }
  return new_cube;
}

int problem1(const layer& initial) {
  std::vector<layer> cube = {initial};
  for (int i = 0; i < 6; ++i) {
    cube = cycle(cube, {}, {});
  }
  return count_active(cube);
}

std::vector<std::vector<layer>> cycle_hyper_cube(const std::vector<std::vector<layer>>& hyper_cube) {
  std::vector<std::vector<layer>> new_hyper_cube;
  std::vector<layer> dummy_cube = std::vector<layer>(
      hyper_cube[0].size(), {hyper_cube[0][0].size(), std::string(hyper_cube[0][0][0].size(), '.')});

  new_hyper_cube.push_back(cycle(dummy_cube, dummy_cube, hyper_cube.back()));
  for (int i = 0; i < hyper_cube.size(); i++) {
    const auto& prev = i == 0 ? dummy_cube : hyper_cube[i - 1];
    const auto& curr = hyper_cube[i];
    const auto& next = i == hyper_cube.size() - 1 ? dummy_cube : hyper_cube[i + 1];
    new_hyper_cube.push_back(cycle(curr, prev, next));
  }
  new_hyper_cube.push_back(cycle(dummy_cube, hyper_cube.back(), dummy_cube));
  return new_hyper_cube;
}

int problem2(const layer& initial) {
  std::vector<std::vector<layer>> hyper_cube = {{initial}};
  for (int i = 0; i < 6; ++i) {
    hyper_cube = cycle_hyper_cube(hyper_cube);
  }

  int result = 0;
  for (const auto& c : hyper_cube) {
    result += count_active(c);
  }
  return result;
}

int main(int argc, const char* argv[]) {
  auto input = read_input("input.txt");
  std::cout << problem1(input) << std::endl;
  std::cout << problem2(input) << std::endl;
  return 0;
}
