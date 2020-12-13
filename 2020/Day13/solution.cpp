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
    } else {
      buses.push_back(-1);
    }
  }
  return {timestamp, buses};
}

int problem1(int timestamp, const std::vector<int>& buses) {
  int min_bus = INT_MAX;
  int bus = -1;
  for (auto b : buses) {
    if (b == -1) {
      continue;
    }
    auto new_time = b - timestamp % b;
    if (new_time < min_bus) {
      min_bus = new_time;
      bus = b;
    }
  }
  return min_bus * bus;
}

long gcm(long a, long b) {
  if (a < b) {
    return gcm(b, a);
  }

  if (a % b == 0) {
    return b;
  }

  return gcm(b, a % b);
}

long lcm(long a, long b) {
  return (a * b) / gcm(a, b);
}

long problem2(const std::vector<int>& buses) {
  std::vector<std::pair<int, int>> departures;
  long current_departure_time = 0;
  for (auto b : buses) {
    if (b != -1) {
      departures.push_back({current_departure_time, b});
    }
    current_departure_time++;
  }

  int b = 0;
  long departure_time_inc = 1;
  for (long departure_time = 1;; departure_time += departure_time_inc) {
    while (b < departures.size() && (departure_time + departures[b].first) % departures[b].second == 0) {
      departure_time_inc = lcm(departure_time_inc, departures[b].second);
      b++;
      if (b == departures.size()) {
        return departure_time;
      }
    }
  }
  throw std::logic_error("Unreachable code reached.");
}

int main(int argc, const char* argv[]) {
  auto [timestamp, buses] = read_input("input.txt");
  std::cout << problem1(timestamp, buses) << std::endl;
  std::cout << problem2(buses) << std::endl;
  return 0;
}
