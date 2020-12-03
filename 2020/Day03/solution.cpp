
#include <iostream>
#include <fstream>
#include <vector>
#include <numeric>

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

int traverse(const std::vector<std::string> &map, int dx, int dy)
{
  int x = 0, y = 0, num_trees = 0;
  while (y < map.size())
  {
    if (map[y][x] == '#')
    {
      num_trees++;
    }
    x = (x + dx) % map[0].size();
    y += dy;
  }
  return num_trees;
}

int problem1(const std::vector<std::string> &map)
{
  return traverse(map, 3, 1);
}

long problem2(const std::vector<std::string> &map)
{
  std::vector<std::pair<int, int>> slopes{
      {1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}};
  return std::accumulate(slopes.begin(), slopes.end(), 1L, [&map](long acc, const auto &slope) {
    return acc * traverse(map, slope.first, slope.second);
  });
}

int main(int argc, const char *argv[])
{
  auto lines = read_lines("input.txt");
  std::cout << problem1(lines) << std::endl;
  std::cout << problem2(lines) << std::endl;
  return 0;
}