
#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <sstream>
#include <algorithm>

std::vector<std::vector<std::string>> read_passports(const std::string &file_name)
{
  std::ifstream file;
  file.open(file_name);
  std::string line;
  std::vector<std::vector<std::string>> passports{{}};
  while (std::getline(file, line))
  {
    if (line == "")
    {
      passports.push_back({});
    }
    else
    {
      passports.back().push_back(line);
    }
  }
  return passports;
}

std::unordered_map<std::string, std::string> parse_passport(const std::vector<std::string> &passport)
{
  std::unordered_map<std::string, std::string> out;
  for (const auto &line : passport)
  {
    std::istringstream ss(line);
    std::string token;
    while (std::getline(ss, token, ' '))
    {
      out[token.substr(0, 3)] = token.substr(4);
    }
  }
  return out;
}

std::vector<std::unordered_map<std::string, std::string>>
parse_passports(const std::vector<std::vector<std::string>> &passports)
{
  std::vector<std::unordered_map<std::string, std::string>> out;
  std::transform(passports.begin(), passports.end(), std::back_inserter(out), parse_passport);
  return out;
}

int problem1(const std::vector<std::unordered_map<std::string, std::string>> &passports)
{
  return std::count_if(passports.begin(), passports.end(), [](const auto &p) {
    return p.size() == 8 || (p.size() == 7 && p.find("cid") == p.end());
  });
}

int main(int argc, const char *argv[])
{
  auto lines = read_passports("input.txt");
  auto passports = parse_passports(lines);
  std::cout << problem1(passports) << std::endl;
  //  std::cout << problem2(lines) << std::endl;
  return 0;
}