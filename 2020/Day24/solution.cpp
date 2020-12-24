#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <set>
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
std::tuple<int, int, int> get_new_pos(std::tuple<int, int, int> pos, const std::string& direction) {
  auto [x, y, z] = pos;
  if (direction == "e") {
    return {x + 1, y - 1, z};
  }
  if (direction == "w") {
    return {x - 1, y + 1, z};
  }
  if (direction == "se") {
    return {x, y - 1, z + 1};
  }
  if (direction == "sw") {
    return {x - 1, y, z + 1};
  }
  if (direction == "ne") {
    return {x + 1, y, z - 1};
  }
  if (direction == "nw") {
    return {x, y + 1, z - 1};
  }
  throw std::logic_error("Invalid direction");
}

std::tuple<int, int, int> get_tile_pos(const std::string& tile) {
  std::tuple<int, int, int> pos = {0, 0, 0};
  for (int i = 0; i < tile.size();) {
    auto dir_length = (tile[i] == 'w' || tile[i] == 'e') ? 1 : 2;
    pos = get_new_pos(pos, tile.substr(i, dir_length));
    i += dir_length;
  }
  return pos;
}

std::set<std::vector<int>> flip_tiles(const std::vector<std::string>& tiles) {
  std::set<std::vector<int>> black_tiles;
  for (const auto& t : tiles) {
    auto [x, y, z] = get_tile_pos(t);
    std::vector<int> key = {x, y, z};
    auto it = black_tiles.find(key);
    if (it == black_tiles.end()) {
      black_tiles.insert(key);
    } else {
      black_tiles.erase(key);
    }
  }
  return black_tiles;
}

int problem1(const std::vector<std::string>& tiles) {
  return flip_tiles(tiles).size();
}

int black_neighbor_tiles(std::tuple<int, int, int> pos, const std::set<std::vector<int>>& black_tiles) {
  int num_black_neighbor_tiles = 0;
  for (auto dir : {"w", "e", "se", "sw", "ne", "nw"}) {
    auto [x, y, z] = get_new_pos(pos, dir);
    if (black_tiles.find({x, y, z}) != black_tiles.end()) {
      num_black_neighbor_tiles++;
    }
  }
  return num_black_neighbor_tiles;
}

int problem2(const std::vector<std::string>& tiles) {
  auto black_tiles = flip_tiles(tiles);
  for (int i = 1; i <= 100; i++) {
    std::set<std::vector<int>> new_floor;
    for (auto t : black_tiles) {
      std::tuple<int, int, int> pos = {t[0], t[1], t[2]};
      auto num_black_neighbor_tiles = black_neighbor_tiles(pos, black_tiles);
      if (num_black_neighbor_tiles > 0 && num_black_neighbor_tiles < 3) {
        new_floor.insert(t);
      }
      for (auto dir : {"w", "e", "se", "sw", "ne", "nw"}) {
        auto new_pos = get_new_pos(pos, dir);
        auto [x, y, z] = new_pos;
        if (black_tiles.find({x, y, z}) == black_tiles.end() &&
            black_neighbor_tiles(new_pos, black_tiles) == 2) {
          new_floor.insert({x, y, z});
        }
      }
    }
    black_tiles = new_floor;
  }
  return black_tiles.size();
}

int main(int argc, const char* argv[]) {
  auto tiles = read_tiles("input.txt");
  std::cout << problem1(tiles) << std::endl;
  std::cout << problem2(tiles) << std::endl;
  return 0;
}