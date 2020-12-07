#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <sstream>
#include <algorithm>
#include <regex>

using graph = std::unordered_map<std::string, std::vector<std::pair<int, std::string>>>;

graph read_bags(const std::string &file_name)
{
  std::ifstream file;
  file.open(file_name);
  std::string line;
  graph bags;
  while (std::getline(file, line))
  {
    std::string from = line.substr(0, line.find(" bags contain "));
    line = line.substr(line.find(" bags contain ") + 14);
    std::vector<std::pair<int, std::string>> contained_bags;
    if (line.find("no other bags.") == std::string::npos)
    {
      std::stringstream ss(line);
      int count;
      std::string b1, b2;
      std::string discard;
      while (ss >> count >> b1 >> b2 >> discard)
      {
        contained_bags.emplace_back(count, b1 + " " + b2);
      }
    }
    bags[from] = contained_bags;
  }
  return bags;
}

void traverse(const std::string &key, const graph &bags, std::vector<std::string> &path,
              std::unordered_set<std::string> &result)
{
  path.push_back(key);
  for (const auto &item : bags.at(key))
  {
    if (item.second == "shiny gold")
    {
      for (const auto &p : path)
      {
        result.insert(p);
      }
    }
    else
    {
      traverse(item.second, bags, path, result);
    }
  }
  path.pop_back();
}

int problem1(const graph &bags)
{
  std::unordered_set<std::string> result;
  std::vector<std::string> path;
  for (const auto item : bags)
  {
    traverse(item.first, bags, path, result);
  }
  return result.size();
}

int count_bags(const std::string &key, const graph &bags)
{
  auto nested = bags.at(key);
  if (nested.empty())
  {
    return 0;
  }
  int result = 0;
  for (const auto &item : nested)
  {
    result += item.first + item.first * count_bags(item.second, bags);
  }
  return result;
}

int problem2(const graph &bags)
{
  return count_bags("shiny gold", bags);
}

int main(int argc, const char *argv[])
{
  auto bags = read_bags("input.txt");
  std::cout << problem1(bags) << std::endl;
  std::cout << problem2(bags) << std::endl;
  return 0;
}
