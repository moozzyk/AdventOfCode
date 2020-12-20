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

std::vector<std::string> expand_rule(
    const std::unordered_map<int, rule>& rules,
    const rule& r,
    std::unordered_map<int, std::vector<std::string>>& cache) {
  auto cached_matches = cache.find(r.id);
  if (cached_matches != cache.end()) {
    return cached_matches->second;
  }

  if (r.terminal.size() > 0) {
    cache[r.id] = {r.terminal};
    return {r.terminal};
  }

  std::vector<std::string> matches;
  for (const auto& child_rules : r.child_rules) {
    std::vector<std::string> partial_matches;
    for (const auto& rule_id : child_rules) {
      auto new_matches = expand_rule(rules, rules.at(rule_id), cache);
      partial_matches = combine_matches(partial_matches, new_matches);
    }
    matches.insert(matches.end(), partial_matches.begin(), partial_matches.end());
  }
  cache[r.id] = matches;
  return matches;
}

size_t match(
    const std::string& message,
    size_t index,
    const rule& current_rule,
    const std::unordered_map<int, rule>& rules) {
  if (index >= message.size()) {
    return std::string::npos;
  }
  if (current_rule.terminal != "") {
    if (message[index] == current_rule.terminal[0]) {
      return index + 1;
    }
    return std::string::npos;
  }

  for (const auto& child_rules : current_rule.child_rules) {
    int new_idx = index;
    for (const auto& child_rule : child_rules) {
      new_idx = match(message, new_idx, rules.at(child_rule), rules);
    }
    if (new_idx != std::string::npos) {
      return new_idx;
    }
  }

  return std::string::npos;
}

int problem1(const std::vector<std::string>& messages, const std::unordered_map<int, rule>& rules) {
  return std::count_if(
      messages.begin(),
      messages.end(),
      [&](const auto& m) { return match(m, 0, rules.at(0), rules) == m.size(); });
}

bool starts_with(const std::string& s, const std::string& prefix) {
  return s.rfind(prefix, 0) == 0;
}
bool ends_with(const std::string& s, const std::string& suffix) {
  return s.size() >= suffix.size() &&
         s.compare(s.size() - suffix.size(), suffix.size(), suffix) == 0;
}

std::pair<int, int> extract_match(
    const std::string& m,
    const std::vector<std::string>& prefixes,
    const std::vector<std::string>& suffixes) {
  for (const auto& p : prefixes) {
    for (const auto& s : suffixes) {
      if (p.size() + s.size() <= m.size() && starts_with(m, p) && ends_with(m, s)) {
        return {p.size(), m.size() - s.size()};
      }
    }
  }
  return {std::string::npos, std::string::npos};
}

bool match_special_11(
    const std::string& m,
    const std::unordered_map<int, std::vector<std::string>>& expanded_rules) {
  const auto& prefixes = expanded_rules.at(42);
  const auto& suffixes = expanded_rules.at(31);
  auto range = extract_match(m, prefixes, suffixes);
  if (range.first == std::string::npos) {
    return false;
  }
  auto partial = m.substr(range.first, range.second - range.first);
  if (partial == "") {
    return true;
  }
  return match_special_11(partial, expanded_rules);
}

std::pair<int, int> match_special_8(
    const std::string& m,
    const std::unordered_map<int, std::vector<std::string>>& expanded_rules) {
  const auto& prefixes = expanded_rules.at(42);
  return extract_match(m, prefixes, {""});
}

bool match_special_0(
    std::string m,
    const std::unordered_map<int, std::vector<std::string>>& expanded_rules) {
  while (m.size() > 0) {
    auto range = match_special_8(m, expanded_rules);
    if (range.first == std::string::npos) {
      return false;
    }
    m = m.substr(range.first, range.second - range.first);
    if (match_special_11(m, expanded_rules)) {
      return true;
    }
  }
  return false;
}

int problem2(
    const std::vector<std::string>& messages,
    const std::unordered_map<int, rule>& rules) {
  std::unordered_map<int, std::vector<std::string>> expanded_rules;
  expand_rule(rules, rules.at(0), expanded_rules);

  int num_matching = 0;
  for (const auto& m : messages) {
    if (match_special_0(m, expanded_rules)) {
      num_matching++;
    }
  }
  return std::count_if(
      messages.begin(),
      messages.end(),
      [&expanded_rules](const auto& m) { return match_special_0(m, expanded_rules); });
}

int main(int argc, const char* argv[]) {
  auto rules = read_rules("rules.txt");
  auto messages = read_messages("messages.txt");
  std::cout << problem1(messages, rules) << std::endl;
  std::cout << problem2(messages, rules) << std::endl;
  return 0;
}