#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <sstream>
#include <algorithm>
#include <regex>

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

std::unordered_map<std::string, std::string> parse_passport(const std::vector<std::string> &passport_lines)
{
  std::unordered_map<std::string, std::string> passport;
  for (const auto &line : passport_lines)
  {
    std::istringstream ss(line);
    std::string token;
    while (std::getline(ss, token, ' '))
    {
      passport[token.substr(0, 3)] = token.substr(4);
    }
  }
  return passport;
}

std::vector<std::unordered_map<std::string, std::string>> parse_passports(
    const std::vector<std::vector<std::string>> &passports_raw)
{
  std::vector<std::unordered_map<std::string, std::string>> passports;
  std::transform(passports_raw.begin(), passports_raw.end(), std::back_inserter(passports), parse_passport);
  return passports;
}

bool basic_check(const std::unordered_map<std::string, std::string> &passport)
{
  return passport.size() == 8 || (passport.size() == 7 && passport.find("cid") == passport.end());
}

bool advanced_check(const std::unordered_map<std::string, std::string> &passport)
{
  if (!basic_check(passport))
  {
    return false;
  }

  for (const auto &entry : passport)
  {
    if (entry.first == "byr")
    {
      auto year = std::stoi(entry.second);
      if (year < 1920 || year > 2002)
        return false;
    }
    else if (entry.first == "iyr")
    {
      auto year = std::stoi(entry.second);
      if (year < 2010 || year > 2020)
        return false;
    }
    else if (entry.first == "eyr")
    {
      auto year = std::stoi(entry.second);
      if (year < 2020 || year > 2030)
        return false;
    }
    else if (entry.first == "hgt")
    {
      auto height = std::stoi(entry.second.substr(0, entry.second.size() - 2));
      if (entry.second.back() == 'm')
      {
        if (height < 150 || height > 193)
          return false;
      }
      else if (height < 59 || height > 76)
        return false;
    }
    else if (entry.first == "hcl")
    {
      if (!std::regex_match(entry.second, std::regex("^#[0-9a-f]{6}$")))
        return false;
    }
    else if (entry.first == "ecl")
    {
      if (!std::regex_match(entry.second, std::regex("^amb|blu|brn|gry|grn|hzl|oth$")))
        return false;
    }
    else if (entry.first == "pid")
    {
      if (!std::regex_match(entry.second, std::regex("^\\d{9}$")))
      {
        return false;
      }
    }
  }
  return true;
}

int problem1(const std::vector<std::unordered_map<std::string, std::string>> &passports)
{
  return std::count_if(passports.begin(), passports.end(), basic_check);
}

int problem2(const std::vector<std::unordered_map<std::string, std::string>> &passports)
{
  return std::count_if(passports.begin(), passports.end(), advanced_check);
}

int main(int argc, const char *argv[])
{
  auto lines = read_passports("input.txt");
  auto passports = parse_passports(lines);
  std::cout << problem1(passports) << std::endl;
  std::cout << problem2(passports) << std::endl;
  return 0;
}