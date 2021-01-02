#include <algorithm>
#include <fstream>
#include <iostream>
#include <list>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

void print_list(const std::list<int>& d, const std::string& sep = " ") {
  for (auto it = d.begin(); it != d.end(); it++) {
    std::cout << *it << sep;
  }
  std::cout << std::endl;
}

void round(std::list<int>& cups, const std::vector<std::list<int>::iterator>& iterators) {
  int dest_cup = cups.front();
  auto range_begin = cups.begin();
  std::advance(range_begin, 1);
  auto range_end = cups.begin();
  std::advance(range_end, +4);
  do {
    dest_cup--;
    if (dest_cup == 0) {
      dest_cup = iterators.size();
    }
  } while (std::find(range_begin, range_end, dest_cup) != range_end);

  auto dest = std::next(iterators[dest_cup - 1]);
  cups.splice(dest, cups, range_begin, range_end);
  cups.splice(cups.end(), cups, cups.begin());
}

std::vector<std::list<int>::iterator> create_iterators(std::list<int>& list) {
  std::vector<std::list<int>::iterator> iterators(list.size(), list.end());
  for (auto it = list.begin(); it != list.end(); it++) {
    iterators[*it - 1] = it;
  }
  return iterators;
}

std::list<int> problem1(const std::vector<int>& c) {
  std::list<int> cups(c.begin(), c.end());
  auto iterators = create_iterators(cups);

  for (int i = 0; i < 100; i++) {
    round(cups, iterators);
  }

  auto pivot = iterators[0];
  cups.splice(cups.end(), cups, cups.begin(), pivot);
  cups.pop_front();
  return cups;
}

unsigned long problem2(const std::vector<int>& c, int max) {
  std::list<int> cups(c.begin(), c.end());
  for (int i = 10; i <= max; i++) {
    cups.emplace_back(i);
  }
  auto iterators = create_iterators(cups);

  for (int i = 0; i < 10'000'000; i++) {
    round(cups, iterators);
  }

  auto n = iterators[0];
  auto n1 = std::next(n);
  auto n2 = std::next(n1);
  return static_cast<unsigned long>(*n1) * static_cast<unsigned long>(*n2);
}

int main(int argc, const char* argv[]) {
  std::string input = "685974213";
  std::vector<int> cups;
  std::transform(
      input.begin(), input.end(), std::back_inserter(cups), [](const auto c) { return c - '0'; });
  print_list(problem1(cups), "");
  std::cout << problem2(cups, 1'000'000) << std::endl;
  return 0;
}