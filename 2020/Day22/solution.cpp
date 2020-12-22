#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

std::vector<std::vector<int>> read_cards(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::string line;
  std::vector<std::vector<int>> decks;
  while (std::getline(file, line)) {
    if (line == "") {
      continue;
    }
    if (line.find("Player") != std::string::npos) {
      decks.push_back({});
    } else {
      decks.back().push_back(std::stoi(line));
    }
  }
  return decks;
}

int problem1(const std::vector<int>& deck1, std::vector<int>& deck2) {
  std::deque<int> d1(deck1.begin(), deck1.end());
  std::deque<int> d2(deck2.begin(), deck2.end());

  while (!(d1.empty() || d2.empty())) {
    auto c1 = d1.front();
    auto c2 = d2.front();
    d1.pop_front();
    d2.pop_front();
    if (c1 > c2) {
      d1.push_back(c1);
      d1.push_back(c2);
    } else {
      d2.push_back(c2);
      d2.push_back(c1);
    }
  }

  const auto& d = d1.empty() ? d2 : d1;
  int i = 1;
  long result = 0;
  for (auto it = d.rbegin(); it != d.rend(); it++, i++) {
    result += i * *it;
  }
  return result;
}

int main(int argc, const char* argv[]) {
  auto decks = read_cards("input.txt");
  std::cout << problem1(decks[0], decks[1]) << std::endl;
  return 0;
}