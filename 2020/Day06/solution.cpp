#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_set>
#include <set>
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

std::set<char> string_to_set(const std::string &str)
{
  std::set<char> s;
  for (char c : str)
  {
    s.insert(c);
  }
  return s;
}

std::set<char> intersect(const std::set<char> &s1, const std::set<char> &s2)
{
  std::set<char> out;
  std::set_intersection(s1.begin(), s1.end(), s2.begin(), s2.end(), std::inserter(out, out.begin()));
  return out;
}

int problem2(const std::vector<std::vector<std::string>> &forms)
{
  int response = 0;
  for (const auto &group_forms : forms)
  {
    auto s = string_to_set(group_forms[0]);
    for (const auto &form : group_forms)
    {
      auto s2 = string_to_set(form);
      s = intersect(s, s2);
    }
    response += s.size();
  }
  return response;
}

int main(int argc, const char *argv[])
{
  auto forms = read_forms("input.txt");
  std::cout << problem1(forms) << std::endl;
  std::cout << problem2(forms) << std::endl;
  return 0;
}
