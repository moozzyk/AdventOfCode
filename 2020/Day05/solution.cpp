#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

std::vector<std::string> read_lines(const std::string &file_name)
{
  std::ifstream file;
  file.open(file_name);
  std::string line;
  std::vector<std::string> lines;
  while (std::getline(file, line))
  {
    lines.push_back(line);
  }
  return lines;
}
int half(const std::string &input, int l, int r)
{

  for (size_t i = 0; i < input.size(); i++)
  {
    if (input[i] == 'F' || input[i] == 'L')
    {
      r = l + ((r - l) >> 1);
    }
    else if (input[i] == 'B' || input[i] == 'R')
    {
      l = l + ((r - l) >> 1) + 1;
    }
  }
  return l;
}

int find_seat(const std::string &line)
{
  int row = half(line.substr(0, 7), 0, 127);
  int seat = half(line.substr(7), 0, 7);
  return row * 8 + seat;
}

int problem1(const std::vector<std::string> &lines)
{
  int max_seat_id = -1;
  for (const auto &line : lines)
  {
    max_seat_id = std::max(max_seat_id, find_seat(line));
  }
  return max_seat_id;
}

int main(int argc, const char *argv[])
{
  auto lines = read_lines("input.txt");
  std::cout << problem1(lines) << std::endl;
  return 0;
}