#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

struct tile_variant {
  tile_variant(const std::vector<std::string>& i) : image(i) {
    for (int i = 0; i < image.size(); i++) {
      top = (top << 1) | (image[0][i] == '#' ? 1 : 0);
      right = (right << 1) | (image[i][image.size() - 1] == '#' ? 1 : 0);
      bottom = (bottom << 1) | (image[image.size() - 1][i] == '#' ? 1 : 0);
      left = (left << 1) | (image[i][0] == '#' ? 1 : 0);
    }
  }

  std::vector<std::string> image;
  int top = 0;
  int right = 0;
  int bottom = 0;
  int left = 0;
};

struct tile {
  int id;
  std::vector<tile_variant> variants;
};

std::unordered_map<int, std::vector<std::string>> read_tiles(const std::string& file_name) {
  std::unordered_map<int, std::vector<std::string>> tiles;
  std::ifstream file;
  file.open(file_name);
  std::string s;
  int id = -1;
  while (file >> s >> id >> s) {
    std::vector<std::string> tile;
    for (int i = 0; i < 10; ++i) {
      file >> s;
      tile.push_back(s);
    }
    tiles[id] = tile;
  }
  return tiles;
}

std::vector<std::string> rotate_right(const std::vector<std::string>& tile_image) {
  auto rotated_tile = tile_image;
  for (int row = 0; row < tile_image.size(); row++) {
    for (int col = 0; col < tile_image.size(); col++) {
      rotated_tile[col][tile_image.size() - 1 - row] = tile_image[row][col];
    }
  }
  return rotated_tile;
}

std::vector<std::string> flip_vertically(const std::vector<std::string>& tile_image) {
  auto flipped_tile = tile_image;
  for (int row = 0; row < tile_image.size(); row++) {
    flipped_tile[tile_image.size() - 1 - row] = tile_image[row];
  }
  return flipped_tile;
}

std::vector<std::string> flip_horizontally(const std::vector<std::string>& tile_image) {
  auto flipped_tile = tile_image;
  for (int row = 0; row < tile_image.size(); row++) {
    for (int col = 0; col < tile_image.size(); col++) {
      flipped_tile[row][tile_image.size() - 1 - col] = tile_image[row][col];
    }
  }
  return flipped_tile;
}

std::vector<tile> prepare_tiles(const std::unordered_map<int, std::vector<std::string>>& tiles_raw) {
  std::vector<tile> tiles;
  for (const auto& [id, image] : tiles_raw) {
    tile t;
    t.id = id;
    auto rotated = image;
    for (int i = 0; i < 4; i++) {
      auto vertical_flip = flip_vertically(rotated);
      auto horizontal_flip = flip_horizontally(rotated);
      t.variants.emplace_back(tile_variant(rotated));
      t.variants.emplace_back(tile_variant(vertical_flip));
      t.variants.emplace_back(tile_variant(horizontal_flip));
      rotated = rotate_right(rotated);
    }
    tiles.push_back(t);
  }
  return tiles;
}

long problem1(const std::vector<tile>& tiles) {
  unsigned long result = 1;
  for (int i = 0; i < tiles.size(); i++) {
    std::unordered_set<int> edges;
    for (int j = 0; j < tiles.size(); j++) {
      if (i == j) {
        continue;
      }
      for (const auto& v : tiles[j].variants) {
        edges.insert(v.top);
        edges.insert(v.right);
        edges.insert(v.bottom);
        edges.insert(v.left);
      }
    }
    const auto& v = tiles[i].variants[0];
    int num_matches = 0;
    for (auto e : {v.top, v.left, v.bottom, v.right}) {
      if (edges.find(e) != edges.end()) {
        num_matches++;
      }
    }
    if (num_matches == 2) {
      std::cout << tiles[i].id << std::endl;
      result *= tiles[i].id;
    }
  }

  return result;
}

int main(int argc, const char* argv[]) {
  auto tiles = prepare_tiles(read_tiles("input.txt"));
  std::cout << problem1(tiles) << std::endl;
  return 0;
}