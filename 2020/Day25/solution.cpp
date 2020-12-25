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

long transform(long subject, int loop_size) {
  long value = 1;
  for (auto i = 0; i < loop_size; i++) {
    value = (value * subject) % 20201227;
  }
  return value;
}

long find_loop_size(int key) {
  int loop_size = 0;
  for (long value = 1; value != key; loop_size++) {
    value = (value * 7) % 20201227;
  }
  return loop_size;
}

int problem1(int card_key, int door_key) {
  auto door_loop_size = find_loop_size(door_key);
  return transform(card_key, door_loop_size);
}

int main(int argc, const char* argv[]) {
  std::cout << problem1(6270530, 14540258) << std::endl;
  return 0;
}