#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_set>
#include <sstream>
#include <algorithm>

std::vector<std::vector<std::string>> read_forms(const std::string &file_name)
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

int problem1(const std::vector<std::vector<std::string>> &forms)
{
  int response = 0;
  for (const auto &group_forms : forms)
  {
    std::unordered_set<char> q;
    for (const auto &form : group_forms)
    {
      for (auto c : form)
      {
        q.insert(c);
      }
    }
    response += q.size();
  }
  return response;
}

int main(int argc, const char *argv[])
{
  auto forms = read_forms("input.txt");
  std::cout << problem1(forms) << std::endl;
  // std::cout << problem2(passports) << std::endl;
  return 0;
}