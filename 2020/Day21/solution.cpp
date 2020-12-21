#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <numeric>
#include <regex>
#include <set>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

std::vector<std::string> split_string(const std::string& s, char delimiter) {
  std::istringstream ss(s);
  std::string token;
  std::vector<std::string> tokens;
  while (std::getline(ss, token, delimiter)) {
    tokens.push_back(token);
  }
  return tokens;
}

std::vector<std::string> split_string(const std::string& s, const std::string& delimiter) {
  auto start = 0U;
  auto end = s.find(delimiter);
  std::vector<std::string> tokens;
  while (end != std::string::npos) {
    tokens.push_back(s.substr(start, end - start));
    start = end + delimiter.length();
    end = s.find(delimiter, start);
  }
  tokens.push_back(s.substr(start, end));
  return tokens;
}

std::vector<std::pair<std::vector<std::string>, std::vector<std::string>>>
read_food_info(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::string line;

  std::vector<std::pair<std::vector<std::string>, std::vector<std::string>>> food_info;
  while (std::getline(file, line)) {
    auto pos = line.find("(");
    auto foods = line.substr(0, pos);
    auto allergens = line.substr(pos + std::string("(contains ").size());
    allergens.pop_back();
    auto food_list = split_string(foods, ' ');
    std::sort(food_list.begin(), food_list.end());
    food_info.emplace_back(food_list, split_string(allergens, ", "));
  }
  return food_info;
}

std::pair<std::map<std::string, std::vector<std::string>>, std::unordered_set<std::string>> build_food_map(
    const std::vector<std::pair<std::vector<std::string>, std::vector<std::string>>>& food_info) {
  std::map<std::string, std::vector<std::string>> food_map;
  std::unordered_set<std::string> good_food_candidates;
  for (const auto& [food_list, allergen_list] : food_info) {
    for (const auto& allergen : allergen_list) {
      auto it = food_map.find(allergen);
      if (it == food_map.end()) {
        food_map[allergen] = food_list;
      } else {
        const auto& existing_list = it->second;
        std::vector<std::string> sym_difference;
        std::vector<std::string> intersection;
        std::set_symmetric_difference(
            food_list.begin(),
            food_list.end(),
            existing_list.begin(),
            existing_list.end(),
            std::back_inserter(sym_difference));
        std::set_intersection(
            food_list.begin(),
            food_list.end(),
            existing_list.begin(),
            existing_list.end(),
            std::back_inserter(intersection));
        it->second = intersection;
        for (const auto& f : sym_difference) {
          good_food_candidates.insert(f);
        }
      }
    }
  }

  return {food_map, good_food_candidates};
}

int problem1(
    const std::vector<std::pair<std::vector<std::string>, std::vector<std::string>>>& food_info,
    const std::map<std::string, std::vector<std::string>>& food_map,
    std::unordered_set<std::string> good_food_candidates) {
  for (const auto& [allergen, food_items] : food_map) {
    for (const auto& f : food_items) {
      good_food_candidates.erase(f);
    }
  }

  int result = 0;
  for (const auto& [food_list, allergen_list] : food_info) {
    for (const auto& f : food_list) {
      if (good_food_candidates.find(f) != good_food_candidates.end()) {
        result++;
      }
    }
  }

  return result;
}

std::string problem2(std::map<std::string, std::vector<std::string>> food_map) {
  std::unordered_set<std::string> used;
  for (auto i = 0; i < food_map.size(); i++) {
    std::string food_to_remove;
    for (auto& [allergen, food_items] : food_map) {
      if (food_items.size() == 1 && used.find(food_items[0]) == used.end()) {
        food_to_remove = food_items.front();
        used.insert(food_to_remove);
        break;
      }
    }
    for (auto it = food_map.begin(); it != food_map.end(); it++) {
      if (it->second.size() != 1) {
        auto pos = std::find(it->second.begin(), it->second.end(), food_to_remove);
        if (pos != it->second.end()) {
          it->second.erase(pos);
        }
      }
    }
  }

  std::stringstream ss;
  for (auto it = food_map.begin(); it != food_map.end(); it++) {
    if (it != food_map.begin()) {
      ss << ",";
    }
    ss << it->second[0];
  }
  return ss.str();
}

int main(int argc, const char* argv[]) {
  auto food_info = read_food_info("input.txt");
  const auto [food_map, good_food_candidates] = build_food_map(food_info);
  std::cout << problem1(food_info, food_map, good_food_candidates) << std::endl;
  std::cout << problem2(food_map) << std::endl;
  return 0;
}