#include <algorithm>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <unordered_set>
#include <vector>

std::pair<int, std::vector<int>> read_input(const std::string& file_name) {
  std::ifstream file;
  int timestamp;
  std::vector<int> buses;
  std::string line;
  file.open(file_name);
  file >> timestamp;
  file >> line;
  std::string token;
  std::istringstream ss(line);
  while (std::getline(ss, token, ',')) {
    if (token != "x") {
      buses.push_back(std::stoi(token));
    }
  }
  return {timestamp, buses};
}

int problem1(int timestamp, const std::vector<int>& buses) {
  int min_bus = INT_MAX;
  int bus = -1;
  for (auto b : buses) {
    auto new_time = b - timestamp % b;
    if (new_time < min_bus) {
      min_bus = new_time;
      bus = b;
    }
  }
  return (min_bus)*bus;
}
int main(int argc, const char* argv[]) {
  auto [timestamp, buses] = read_input("input.txt");
  std::cout << problem1(timestamp, buses) << std::endl;
  return 0;
}
