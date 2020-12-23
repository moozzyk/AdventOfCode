#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

void print_deque(const std::deque<int>& d, const std::string& sep = " ") {
  for (auto it = d.begin(); it != d.end(); it++) {
    std::cout << *it << sep;
  }
  std::cout << std::endl;
}

void round(std::deque<int>& cups) {
  int dest_cup = cups.front();
  do {
    dest_cup--;
    if (dest_cup == 0) {
      dest_cup = 9;
    }
  } while (std::find(cups.begin() + 1, cups.begin() + 4, dest_cup) != cups.begin() + 4);

  auto dest = std::find(cups.begin(), cups.end(), dest_cup);
  cups.insert(dest + 1, cups.begin() + 1, cups.begin() + 4);
  cups.push_back(cups.front());
  cups.erase(cups.begin(), cups.begin() + 4);
}

std::deque<int> problem1(const std::vector<int>& c) {
  std::deque<int> cups(c.begin(), c.end());
  for (int i = 0; i < 100; i++) {
    round(cups);
  }

  auto pivot = std::find(cups.begin(), cups.end(), 1);
  cups.insert(cups.end(), cups.begin(), pivot);
  cups.erase(cups.begin(), pivot + 1);
  return cups;
}

int main(int argc, const char* argv[]) {
  std::string input = "685974213";
  std::vector<int> cups;
  std::transform(
      input.begin(), input.end(), std::back_inserter(cups), [](const auto c) { return c - '0'; });
  print_deque(problem1(cups), "");
  return 0;
}