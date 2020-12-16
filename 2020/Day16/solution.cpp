#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using ticket = std::vector<int>;

struct rule {
  int rule_id;
  std::string name;
  std::pair<int, int> range1;
  std::pair<int, int> range2;
};

std::vector<rule> read_rules(const std::string& file_name) {
  std::string name, discard;
  char discard_char;
  std::pair<int, int> range1, range2;
  std::vector<rule> rules;
  std::ifstream file;
  file.open(file_name);
  int rule_id = 0;
  while (file >> name) {
    if (name[name.size() - 1] != ':') {
      file >> discard;
      name += " " + discard;
    }
    name.pop_back();
    file >> range1.first >> discard_char >> range1.second;
    file >> discard;
    file >> range2.first >> discard_char >> range2.second;
    rules.push_back({rule_id, name, range1, range2});
    rule_id++;
  }
  return rules;
}

std::vector<ticket> read_tickets(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::string line;
  std::vector<ticket> tickets;
  while (std::getline(file, line)) {
    std::istringstream ss(line);
    std::string token;
    ticket t;
    while (std::getline(ss, token, ',')) {
      t.push_back(std::stoi(token));
    }
    tickets.push_back(t);
  }
  return tickets;
}

int get_error_rate(const ticket& t, const std::vector<rule>& rules) {
  for (const auto n : t) {
    int valid_rules = 0;
    for (auto r : rules) {
      if ((n >= r.range1.first && n <= r.range1.second) ||
          (n >= r.range2.first && n <= r.range2.second)) {
        valid_rules++;
      }
    }
    if (valid_rules == 0) {
      return n;
    }
  }
  return 0;
}

int problem1(const std::vector<rule>& rules, const std::vector<ticket>& nearby_tickets) {
  int error_rate = 0;
  for (const auto& t : nearby_tickets) {
    error_rate += get_error_rate(t, rules);
  }
  return error_rate;
}

int main(int argc, const char* argv[]) {
  auto rules = read_rules("rules.txt");
  auto nearby_tickets = read_tickets("nearby_tickets.txt");
  auto my_ticket = read_tickets("ticket.txt").front();
  std::cout << problem1(rules, nearby_tickets) << std::endl;
  return 0;
}
