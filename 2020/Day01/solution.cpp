#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_set>

std::vector<int> readFile()
{
  std::ifstream file;
  file.open("input.txt");

  int i;
  std::vector<int> v;
  while (file >> i)
  {
    v.push_back(i);
  }
  return v;
}

int problem1(const std::vector<int> input)
{
  std::unordered_set<int> numbers;
  for (const auto n : input)
  {
    if (numbers.find(2020 - n) != numbers.end())
    {
      std::cout << n << std::endl;
      return n * (2020 - n);
    }
    numbers.insert(n);
  }

  return -1;
}

int main(int argc, const char *argv[])
{
  auto numbers = readFile();
  std::cout << problem1(numbers) << std::endl;
}
