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
  int id;
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
  int id = 0;
  while (file >> name) {
    if (name[name.size() - 1] != ':') {
      file >> discard;
      name += " " + discard;
    }
    name.pop_back();
    file >> range1.first >> discard_char >> range1.second;
    file >> discard;
    file >> range2.first >> discard_char >> range2.second;
    rules.push_back({id, name, range1, range2});
    id++;
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
  return -1;
}

int problem1(const std::vector<rule>& rules, const std::vector<ticket>& nearby_tickets) {
  int error_rate = 0;
  for (const auto& t : nearby_tickets) {
    auto n = get_error_rate(t, rules);
    if (n >= 0) {
      error_rate += get_error_rate(t, rules);
    }
  }
  return error_rate;
}

void invalidate_fields(
    const std::vector<rule>& rules,
    std::vector<std::unordered_set<int>>& field_rules,
    const ticket& t) {
  for (int i = 0; i < t.size(); i++) {
    int n = t[i];
    for (auto r : rules) {
      if (!((n >= r.range1.first && n <= r.range1.second) ||
            (n >= r.range2.first && n <= r.range2.second))) {
        field_rules[i].erase(r.id);
      }
    }
  }
}

std::vector<int> find_mappings(std::vector<std::unordered_set<int>> field_rules) {
  std::vector<int> mapping(field_rules.size(), -1);
  for (auto c = 0; c < field_rules.size(); c++) {
    int i = 0;
    for (; i < field_rules.size(); i++) {
      if (field_rules[i].size() == 1) {
        break;
      }
    }
    auto rule_id = *(field_rules[i].begin());
    mapping[i] = rule_id;
    for (auto& s : field_rules) {
      s.erase(rule_id);
    }
  }
  return mapping;
}

long problem2(const std::vector<rule>& rules, const std::vector<ticket>& nearby_tickets, const ticket& my_ticket) {
  std::vector<std::unordered_set<int>> field_rules(my_ticket.size());
  for (auto i = 0; i < my_ticket.size(); i++) {
    for (auto r : rules) {
      field_rules[i].insert(r.id);
    }
  }

  for (const auto& t : nearby_tickets) {
    if (get_error_rate(t, rules) != -1) {
      continue;
    }
    invalidate_fields(rules, field_rules, t);
  }

  auto mappings = find_mappings(field_rules);
  long result = 1;
  for (int i = 0; i < mappings.size(); i++) {
    if (mappings[i] < 6) {
      result *= my_ticket[i];
    }
  }
  return result;
}

int main(int argc, const char* argv[]) {
  auto rules = read_rules("rules.txt");
  auto nearby_tickets = read_tickets("nearby_tickets.txt");
  auto my_ticket = read_tickets("ticket.txt").front();
  std::cout << problem1(rules, nearby_tickets) << std::endl;
  std::cout << problem2(rules, nearby_tickets, my_ticket) << std::endl;
  return 0;
}