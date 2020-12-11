#include <algorithm>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <unordered_set>
#include <vector>

std::vector<std::string> read_seats(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::vector<std::string> seats;
  std::string tmp;
  while (file >> tmp) {
    seats.push_back(tmp);
  }
  return seats;
}

int num_occupied(const std::vector<std::string>& seats, int row, int col) {
  int result = 0;
  for (int y = row - 1; y <= row + 1; y++) {
    for (int x = col - 1; x <= col + 1; x++) {
      if (y == row && x == col) {
        continue;
      }
      if (x >= 0 && y >= 0 && y < seats.size() && x < seats[y].size()) {
        if (seats[y][x] == '#') {
          result++;
        }
      }
    }
  }
  return result;
}

std::vector<std::string> round(const std::vector<std::string>& seats) {
  std::vector<std::string> result = seats;
  for (int row = 0; row < seats.size(); row++) {
    for (int col = 0; col < seats[row].size(); col++) {
      auto num = num_occupied(seats, row, col);
      if (seats[row][col] == 'L' && num == 0) {
        result[row][col] = '#';
      } else if (seats[row][col] == '#' && num >= 4) {
        result[row][col] = 'L';
      } else {
        result[row][col] = seats[row][col];
      }
    }
  }
  return result;
}

int problem1(const std::vector<std::string>& initial) {
  auto seats = initial;
  for (int r = 0;; r++) {
    auto new_seats = round(seats);
    if (seats == new_seats) {
      int result = 0;
      for (auto l : new_seats) {
        result += std::count_if(l.begin(), l.end(), [](char c) { return c == '#'; });
      }
      return result;
    }
    seats = new_seats;
  }
  throw std::logic_error("Unreachable code reached");
}

int main(int argc, const char* argv[]) {
  auto numbers = read_seats("input.txt");
  std::cout << problem1(numbers) << std::endl;
  return 0;
}
