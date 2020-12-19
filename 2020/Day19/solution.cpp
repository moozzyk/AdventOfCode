#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

struct rule {
  int id;
  std::string terminal;
  std::vector<std::vector<int>> child_rules;
};

std::vector<int> read_numbers(const std::string& i) {
  std::vector<int> ret;
  std::string s;
  std::istringstream ss(i);
  while (ss >> s) {
    ret.push_back(std::stoi(s));
  }
  return ret;
}

std::unordered_map<int, rule> read_rules(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::string line;
  std::unordered_map<int, rule> rules;
  while (std::getline(file, line)) {
    rule r;
    r.terminal = "";
    r.id = std::stoi(line.substr(0, line.find(":")));
    if (line.find("a") != std::string::npos) {
      r.terminal = "a";
    } else if (line.find("b") != std::string::npos) {
      r.terminal = "b";
    } else {
      auto t = line.substr(line.find(":") + 1);
      auto pos = t.find("|");
      auto g = t.substr(0, pos);
      r.child_rules.push_back(read_numbers(g));
      if (pos != std::string::npos) {
        g = t.substr(pos + 1);
        r.child_rules.push_back(read_numbers(g));
      }
    }
    rules[r.id] = r;
  }
  return rules;
}

std::vector<std::string> read_messages(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::string line;
  std::vector<std::string> lines;
  while (std::getline(file, line)) {
    lines.push_back(line);
  }
  return lines;
}

std::vector<std::string> combine_matches(const std::vector<std::string>& ss1, const std::vector<std::string>& ss2) {
  if (ss1.empty()) {
    return ss2;
  }
  if (ss2.empty()) {
    return ss1;
  }
  std::vector<std::string> result;
  result.reserve(ss1.size() * ss2.size());

  for (const auto& s1 : ss1) {
    for (const auto& s2 : ss2) {
      result.push_back(s1 + s2);
    }
  }
  return result;
}

std::vector<std::string> expand_rule(const std::unordered_map<int, rule>& rules, rule r) {
  if (r.terminal.size() > 0) {
    return {r.terminal};
  }

  std::vector<std::string> matches;
  for (const auto& child_rules : r.child_rules) {
    std::vector<std::string> partial_matches;
    for (const auto& rule_id : child_rules) {
      auto new_matches = expand_rule(rules, rules.at(rule_id));
      partial_matches = combine_matches(partial_matches, new_matches);
    }
    matches.insert(matches.end(), partial_matches.begin(), partial_matches.end());
  }
  return matches;
}

int problem1(const std::unordered_map<int, rule>& rules, const std::vector<std::string>& messages) {
  auto possible_matches = expand_rule(rules, rules.at(0));
  std::unordered_set<std::string> set;
  for (const auto& s : possible_matches) {
    set.insert(s);
  }

  auto result = 0;
  for (const auto& m : messages) {
    if (set.find(m) != set.end()) {
      result++;
    }
  }
  return result;
}

int main(int argc, const char* argv[]) {
  auto rules = read_rules("rules.txt");
  auto messages = read_messages("messages.txt");
  std::cout << problem1(rules, messages) << std::endl;
  return 0;
}