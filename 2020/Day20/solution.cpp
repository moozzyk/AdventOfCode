#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric>
#include <regex>
#include <set>
#include <sstream>
#include <unordered_map>
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
  std::unordered_map<int, std::vector<std::string>> tiles{};
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

std::vector<tile> find_corner_tiles(const std::vector<tile>& tiles) {
  std::vector<tile> corner_tiles;
  for (int i = 0; i < tiles.size(); i++) {
    std::set<int> edges;
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
      corner_tiles.push_back(tiles[i]);
    }
  }
  return corner_tiles;
}

long problem1(const std::vector<tile>& tiles) {
  auto corner_tiles = find_corner_tiles(tiles);
  return std::accumulate(
      corner_tiles.begin(),
      corner_tiles.end(),
      1L,
      [](long acc, const auto& t) {
        return acc * t.id;
      });
}

std::set<std::pair<int, int>> intersect(
    const std::set<std::pair<int, int>>& s1,
    const std::set<std::pair<int, int>>& s2) {
  std::set<std::pair<int, int>> intersection;
  for (const auto& i : s1) {
    if (s2.find(i) != s2.end()) {
      intersection.insert(i);
    }
  }
  return intersection;
}

bool build_map(
    std::vector<std::pair<int, int>>& map,
    std::set<int>& used_tiles,
    std::unordered_map<int, std::set<std::pair<int, int>>>& edge_to_tile,
    const std::vector<tile>& tiles) {
  if (map.size() == tiles.size()) {
    return true;
  }
  int board_size = std::sqrt(tiles.size());
  int row = map.size() / board_size;
  int col = map.size() % board_size;
  assert(row > 0 || col > 0);

  int left_side = -1, top_side = -1;
  std::set<std::pair<int, int>> candidates{};
  if (col > 0) {
    left_side = tiles[map.back().first].variants[map.back().second].right;
    for (const auto& p : edge_to_tile.at(left_side)) {
      if (used_tiles.find(p.first) == used_tiles.end()) {
        candidates.insert(p);
      }
    }
  }

  if (row > 0) {
    top_side = tiles[map[map.size() - board_size].first].variants[map[map.size() - board_size].second].bottom;
    for (const auto& p : edge_to_tile.at(top_side))
      if (used_tiles.find(p.first) == used_tiles.end()) {
        candidates.insert(p);
      }
  }

  for (auto [tile_idx, variant_idx] : candidates) {
    const auto& tile_variant = tiles[tile_idx].variants[variant_idx];
    if ((left_side != -1 && tile_variant.left != left_side) ||
        (top_side != -1 && tile_variant.top != top_side)) {
      continue;
    }

    used_tiles.insert(tile_idx);
    map.push_back({tile_idx, variant_idx});
    if (build_map(map, used_tiles, edge_to_tile, tiles)) {
      return true;
    }
    map.pop_back();
    used_tiles.erase(tile_idx);
  }
  return false;
}

std::vector<std::pair<int, int>> get_tile_layout(const std::vector<tile> tiles) {
  std::unordered_map<int, std::set<std::pair<int, int>>> edge_to_tile{};
  for (int i = 0; i < tiles.size(); i++) {
    for (int j = 0; j < tiles[i].variants.size(); j++) {
      edge_to_tile[tiles[i].variants[j].top].insert({i, j});
      edge_to_tile[tiles[i].variants[j].right].insert({i, j});
      edge_to_tile[tiles[i].variants[j].bottom].insert({i, j});
      edge_to_tile[tiles[i].variants[j].left].insert({i, j});
    }
  }
  auto starting_tile = find_corner_tiles(tiles)[0];
  auto it = std::find_if(tiles.begin(), tiles.end(), [&starting_tile](const auto& t) {
    return t.id == starting_tile.id;
  });
  auto starting_tile_idx = static_cast<int>(it - tiles.begin());

  std::vector<std::pair<int, int>> map{};
  std::set<int> used_tiles = {starting_tile_idx};
  for (int v = 0; v < starting_tile.variants.size(); v++) {
    map.emplace_back(starting_tile_idx, v);
    if (build_map(map, used_tiles, edge_to_tile, tiles)) {
      break;
    }
    map.pop_back();
  }

  return map;
}

std::vector<std::string> build_map(
    const std::vector<std::pair<int, int>>& tile_layout,
    const std::vector<tile>& tiles) {
  int side = std::sqrt(tiles.size());
  // 8 => tile size (10) - 2 (top and bottom row removed)
  auto tile_size = 8;
  std::vector<std::string> map(side * tile_size, "");
  for (auto i = 0; i < tiles.size(); i++) {
    auto [tile_idx, tile_variant_idx] = tile_layout[i];
    auto tile_variant = tiles[tile_idx].variants[tile_variant_idx];
    for (auto j = 0; j < tile_size; j++) {
      // j + 1 skip top edge
      auto line = tile_variant.image[j + 1].substr(1, tile_size);
      map[(i / side) * tile_size + j].append(line);
    }
  }
  return map;
}

bool is_match(int pos, const std::string& s, const std::string& target) {
  if (pos + target.size() > s.size()) {
    return false;
  }
  for (auto i = 0; i < target.size(); i++) {
    if (target[i] == '#' && s[pos + i] != '#') {
      return false;
    }
  }
  return true;
}

const std::vector<std::string> monster{
    "..................#.",
    "#....##....##....###",
    ".#..#..#..#..#..#..."};

void mark_monster(std::vector<std::string>& map, int row, int col) {
  for (int i = 0; i < monster.size(); i++) {
    for (int j = 0; j < monster[i].size(); j++) {
      if (monster[i][j] == '#') {
        map[row + i][col + j] = 'O';
      }
    }
  }
}

bool mark_monsters_on_map(std::vector<std::string>& map) {
  bool found = false;
  for (int row = 1; row < map.size() - 1; row++) {
    for (int col = 0; col < map[row].size() - monster.front().size(); col++) {
      if (is_match(col, map[row], monster[1]) &&
          is_match(col, map[row - 1], monster[0]) &&
          is_match(col, map[row + 1], monster[2])) {
        found = true;
        mark_monster(map, row - 1, col);
      }
    }
  }
  return found;
}

bool mark_monsters(std::vector<std::string> map, std::vector<std::string>& result) {
  for (int i = 0; i < 4; i++) {
    result = map;
    if (mark_monsters_on_map(result)) {
      return true;
    }
    result = flip_vertically(map);
    if (mark_monsters_on_map(result)) {
      return true;
    }
    result = flip_horizontally(map);
    if (mark_monsters_on_map(result)) {
      return true;
    }
    map = rotate_right(map);
  }
  return false;
}

int find_water_roughness(const std::vector<std::string>& map) {
  std::vector<std::string> map_with_monsters;
  if (mark_monsters(map, map_with_monsters)) {
    return std::accumulate(map_with_monsters.begin(), map_with_monsters.end(), 0, [](int acc, const auto& s) {
      return acc + std::count_if(s.begin(), s.end(), [](auto c) {
               return c == '#';
             });
    });
  }
  return -1;
}

int problem2(const std::vector<tile>& tiles) {
  auto tile_layout = get_tile_layout(tiles);
  assert(tile_layout.size() > 0);
  auto map = build_map(tile_layout, tiles);
  return find_water_roughness(map);
}

int main(int argc, const char* argv[]) {
  auto tiles = prepare_tiles(read_tiles("input.txt"));
  std::cout << problem1(tiles) << std::endl;
  std::cout << problem2(tiles) << std::endl;
  return 0;
}
