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

int traverse_direction(const std::vector<std::string>& seats, int x, int y, int dy, int dx) {
  int result = 0;
  while (true) {
    x += dx;
    y += dy;
    if (!(x >= 0 && y >= 0 && x < seats[y].size() && y < seats.size())) {
      return 0;
    }
    if (seats[y][x] == 'L') {
      return 0;
    }
    if (seats[y][x] == '#') {
      return 1;
    }
  }
}

int new_num_occupied(const std::vector<std::string>& seats, int row, int col) {
  return traverse_direction(seats, col, row, -1, -1) +
         traverse_direction(seats, col, row, -1, 0) +
         traverse_direction(seats, col, row, -1, 1) +
         traverse_direction(seats, col, row, 0, 1) +
         traverse_direction(seats, col, row, 1, 1) +
         traverse_direction(seats, col, row, 1, 0) +
         traverse_direction(seats, col, row, 1, -1) +
         traverse_direction(seats, col, row, 0, -1);
}

std::vector<std::string> round(
    const std::vector<std::string>& seats, int occupancy,
    std::function<int(const ::std::vector<std::string>&, int, int)> num_occupied_fn) {
  std::vector<std::string> result = seats;
  for (int row = 0; row < seats.size(); row++) {
    for (int col = 0; col < seats[row].size(); col++) {
      auto num = num_occupied_fn(seats, row, col);
      if (seats[row][col] == 'L' && num == 0) {
        result[row][col] = '#';
      } else if (seats[row][col] == '#' && num >= occupancy) {
        result[row][col] = 'L';
      } else {
        result[row][col] = seats[row][col];
      }
    }
  }
  return result;
}

std::vector<std::string> rounds(
    const std::vector<std::string>& initial, int occupancy,
    std::function<int(const ::std::vector<std::string>&, int, int)> num_occupied_fn) {
  auto seats = initial;
  while (true) {
    auto new_seats = round(seats, occupancy, num_occupied_fn);
    if (seats == new_seats) {
      return seats;
    }
    seats = new_seats;
  }
}

int count_seats(const std::vector<std::string>& seats) {
  int result = 0;
  for (const auto& l : seats) {
    result += std::count_if(l.begin(), l.end(), [](char c) { return c == '#'; });
  }
  return result;
}

int problem1(const std::vector<std::string>& initial) {
  return count_seats(rounds(initial, 4, num_occupied));
}

int problem2(const std::vector<std::string>& initial) {
  return count_seats(rounds(initial, 5, new_num_occupied));
}

int main(int argc, const char* argv[]) {
  auto seats = read_seats("input.txt");
  std::cout << problem1(seats) << std::endl;
  std::cout << problem2(seats) << std::endl;
  return 0;
}
