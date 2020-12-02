
#include <iostream>
#include <fstream>
#include <vector>

struct password_line
{
  int min_length;
  int max_length;
  char letter;
  std::string password;
};

std::vector<password_line> read_lines(const std::string &file_name)
{
  std::ifstream file;
  file.open(file_name);
  std::string line;
  std::vector<password_line> lines;
  while (!file.eof())
  {
    char discard;
    password_line p;
    file >> p.min_length >> discard >> p.max_length >> p.letter >> discard >> p.password;
    lines.push_back(p);
  }
  return lines;
}

bool is_valid(const password_line &p)
{
  auto count = std::count_if(p.password.begin(), p.password.end(), [&p](char c) {
    return c == p.letter;
  });
  return count >= p.min_length and count <= p.max_length;
}

int problem1(const std::vector<password_line> &lines)
{
  return std::count_if(lines.begin(), lines.end(), is_valid);
}

int problem2(const std::vector<password_line> &lines)
{
  return std::count_if(lines.begin(), lines.end(), [](const auto &p) {
    auto l = p.password[p.min_length - 1];
    auto r = p.password[p.max_length - 1];
    return l == p.letter ^ r == p.letter;
  });
}

int main(int argc, const char *argv[])
{
  auto lines = read_lines("input.txt");
  std::cout << problem1(lines) << std::endl;
  std::cout << problem2(lines) << std::endl;
  return 0;
}