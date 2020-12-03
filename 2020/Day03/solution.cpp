
#include <iostream>
#include <fstream>
#include <vector>

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
    y++;
  }
  return num_trees;
}

int problem1(const std::vector<std::string> &map)
{
  return traverse(map, 3, 1);
}

int main(int argc, const char *argv[])
{
  auto lines = read_lines("input.txt");
  std::cout << problem1(lines) << std::endl;
  return 0;
}