#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

std::vector<std::string> read_tiles(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::string line;
  std::vector<std::string> tiles;
  while (file >> line) {
    tiles.push_back(line);
  }
  return tiles;
}

// x => \, y => -, z => /
std::tuple<int, int, int> get_tile_pos(const std::string& tile) {
  int x = 0, y = 0, z = 0;
  for (int i = 0; i < tile.size(); i++) {
    switch (tile[i]) {
      case 'e':
        x++;
        y--;
        break;
      case 'w':
        x--;
        y++;
        break;
      case 's':
        if (tile[i + 1] == 'e') {
          z++;
          y--;
        } else {
          x--;
          z++;
        }
        i++;
        break;
      case 'n':
        if (tile[i + 1] == 'e') {
          x++;
          z--;
        } else {
          z--;
          y++;
        }
        i++;
        break;
      default:
        throw std::logic_error("Invalid direction");
    }
  }
  return {x, y, z};
}

int problem1(const std::vector<std::string>& tiles) {
  std::unordered_set<std::string> black_tiles;
  for (const auto& t : tiles) {
    auto [x, y, z] = get_tile_pos(t);
    auto key = std::to_string(x) + "_" + std::to_string(y) + "_" + std::to_string(z);
    auto it = black_tiles.find(key);
    if (it == black_tiles.end()) {
      black_tiles.insert(key);
    } else {
      black_tiles.erase(key);
    }
  }

  return black_tiles.size();
}

int main(int argc, const char* argv[]) {
  auto tiles = read_tiles("input.txt");
  std::cout << problem1(tiles) << std::endl;
  return 0;
}